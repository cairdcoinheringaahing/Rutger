import collections
import functools
import inspect
import itertools
import math
import sys

try: import regex as re
except: import re

ARGV = itertools.cycle(sys.argv[2:])

class Operator:
    def __init__(self, op = None, arg = None):
        self.op = op
        self.arg = arg

    def __repr__(self):
        return 'Operator(op = {}, arg = {})'.format(self.op, self.arg)

    def __iter__(self):
        return iter([self.op, self.arg])

class Assignment:
    def __init__(self, var = None, val = None):
        self.var = var
        self.val = val

    def __repr__(self):
        return 'Assignment({} = {})'.format(self.var, self.val)

class Block:
    def __init__(self, *code):
        self.code = code

    def __repr__(self):
        return 'Block({})'.format(', '.join(map(str, self.code)))

    def __iter__(self):
        return iter(self.code)

class Msg(Exception):
    def __init__(self, err, message):
        self.message = message
        self.err = err
        super().__init__(message)

class Error(Exception):
    def __init__(self, error_type, line, index, msg = None):
        if msg is not None:
            message = '''{}Error on line {}:

    {}

{}'''.format(error_type, index, line, msg)
        else:
            message = '''{}Error on line {}:

    {}

'''.format(error_type, index, line)

        super().__init__(message)

def statements(array):
    states = [[]]
    block = 0
    for tkn in array:
        if tkn == '{':
            block += 1
        if tkn == '}':
            block -= 1
            
        if tkn == ';' and not block:
            states.append([])
        else:
            states[-1].append(tkn)
    return list(filter(None, states))

def tokenise(string):
    tokens = ['']
    index = 0
    comment = False

    while index < len(string):
        char = string[index]

        if comment and char != '\n':
            index += 1

        elif char == '"':
            tokens.append('')
            prev = '\\'
            while index < len(string):
                if char == '"' and prev != '\\':
                    break
                tokens[-1] += char
                index += 1
                prev = char
                char = string[index]
            tokens[-1] += '"'
            index += 1

        elif char == "'":
            tokens.append('')
            prev = '\\'
            while index < len(string):
                if char == "'" and prev != '\\':
                    break
                tokens[-1] += char
                index += 1
                prev = char
                char = string[index]
            tokens[-1] += "'"
            index += 1

        elif index < len(string)-2 and string[index:index+2] == '//':
            comment = True

        elif char.isalpha() or char in '$@':
            
            if char in '$@':
                tokens.append(char)
                index += 1
                char = string[index]
            else:
                tokens.append('')
                
            while char.isalpha() and index < len(string):
                tokens[-1] += char
                index += 1
                char = string[index]

        elif char == '-':
            tokens.append('-')
            index += 1
            while index < len(string) and string[index].isdigit():
                tokens[-1] += string[index]
                index += 1

            if string[index] == '.':
                tokens[-1] += '.'
                index += 1
                while index < len(string) and string[index].isdigit():
                    tokens[-1] += string[index]
                    index += 1

        elif char.isdigit():
            tokens.append('')
            while char.isdigit() and index < len(string):
                tokens[-1] += char
                index += 1
                if index == len(string):
                    break
                char = string[index]

            if char == '.':
                tokens[-1] += char
                index += 1
                while index < len(string) and string[index].isdigit():
                    tokens[-1] += string[index]
                    index += 1

        elif char == '(':
            tokens.append('')
            depth = 0
            once = True
            while depth or once:
                once = False
                if char == '(':
                    depth += 1
                if char == ')':
                    depth -= 1
                tokens[-1] += char
                index += 1
                if index == len(string):
                    break
                char = string[index]

        elif char == '\n':
            comment = False
            index += 1

        elif char == '\t':
            index += 1

        else:
            if tokens[-1]:
                tokens.append(char)
            else:
                tokens[-1] += char

            index += 1

    return list(filter(None, tokens))

def parse(string):
    tkns = tokenise(string)
    segments = statements(tkns)

    def reduce(array):
        original = ''.join(array.copy())
        array = list(filter(lambda a: a != ' ', array))

        if len(array) == 1:
            array = array[0]
            if array[0] == '(' and array[-1] == ')':
                return to_array(array)
            else:
                try: return eval(array)
                except: return array
        
        elif array[1] == '=':
            var = array.pop(0)
            if var[0] in '$@':
                raise Error('Syntax', original, index + 1, 'Variable names may not start with $ or @ in assignment')
            base = Assignment(var = var)
            array.pop(0)
            base.val = reduce(array)
            return base

        elif '{' in array and '}' in array:
            func, _, _, *code, _, _ = array
            reduc = Block(*list(map(reduce, statements(code))))
            return Operator(op = func, arg = reduc)

        else:
            if 'Function' in array[1:]:
                raise Error('Syntax', original, index + 1, 'Function declerations may not be arguments')

            if array[0] == 'Function':
                start = array.index('[') + 1
                end = array.index(']')
                sig = ''.join(array[start : end])
                length = end - start - 1
                array[start] = sig
                for _ in range(length):
                    array.pop(-2)
            
            opers = []
            for tkn in array:
                if re.search(r'^[A-Za-z]+$', tkn):
                    opers.append(Operator(op = tkn))
                    array.remove(tkn)
                
            array = list(filter(lambda a: a not in '[]', array))
            
            if len(array) > 1:
                raise Error('Syntax', original, index + 1, 'Curried functions must be declared before currying')

            if len(array) == 0:
                raise Error('Syntax', original, index + 1, 'Non function variables must be prefixed with a \'$\' in arguments')

            base = opers.pop()
            base.arg = array.pop()
            while opers:
                opers[-1].arg = base
                base = opers.pop()
            return base

    string = list(map(lambda a: a.split('//')[0], string.split('\n')))

    for index, segment in enumerate(segments):
        if string[index] and string[index].strip()[-1] not in ';{[':
            raise Error('Syntax', string[index], index, 'Missing semicolon')
   
        yield (index, reduce(segment))

def to_array(value):
    ret = []
    parsed = tokenise(value[1:-1])
    for elem in parsed:
        if elem[0] == '(' and elem[-1] == ')':
            ret.append(to_array(elem))
        elif elem != ' ':
            ret.append(eval(elem))
    return ret

def sig_parse(sig):
    try:
        sig = sig.split('<', 1)[1].strip('>')
    except:
        pass
    return re.split(r'\s*->\s*', sig)

def getvar(name):
    if not isinstance(name, str):
        return name
    
    try:
        return eval(name)
    except:
        pass
    
    if name == '$Input':
        ret = input()
        try: return eval(ret)
        except: return ret
    elif name == '$Argv':
        return next(ARGV)
    
    elif name[1:] in variables:
        return variables[name[1:]]
    elif name[1:] in builtins:
        return builtins[name[1:]]
        
def call(func, code, line):
    func, arg = func
    try:
        arg = eval(arg)
    except:
        pass

    if isinstance(arg, str) and arg[0] == '$':
        arg = getvar(arg)
        if callable(arg) and func == 'Print':
            arg = arg.sig
    
    if func in builtins.keys():
        func = builtins[func]
        return func(arg, code, line)
    
    if func in variables.keys():
        return variables[func](arg, code, line)
    
    raise Error('Name', code, line, 'Unknown function: {}'.format(func))

def fromassign(func, code, line):
    if isinstance(func, Operator):
        return call(func, code, line)
    
    if isinstance(func, Assignment):
        ret = fromassign(func.val, code, line)
        variables[func.var] = ret
        return ret
    
    if isinstance(func, str) and func[0] == '$':
        return getvar(func)

    try:
        return eval(func)
    except:
        return func

def execute(code):
    code = code.strip()
    tree = list(parse(code))

    returns = []
    for ln, ins in tree:
        line = code.split('\n')[ln]
        if isinstance(ins, Operator):
            returns.append(call(ins, line, ln))
        if isinstance(ins, Assignment):
            variables[ins.var] = fromassign(ins.val, line, ln)

def evaluate(block, line, num):
    returns = []
    for chunk in block:
        if isinstance(chunk, Operator):
            returns.append(call(chunk, line, num))
        if isinstance(chunk, Assignment):
            variables[chunk.var] = fromassign(chunk.val, line, num)
    return returns

def to_arg(arg, line, ln):
    if isinstance(arg, Operator):
        arg = call(arg, line, ln)
        
    if isinstance(arg, str) and arg[0] == '$':
        arg = getvar(arg)
    return arg

def wrappers(func, sig, wrapper = 'simple'):
    
    def identity(arg, code, line):
        arg = to_arg(arg, code, line)
        try:
            check_type(sig, arg)
            func(arg)
        except Msg as m:
            raise Error(m.err, code, line, m.message)
        except TypeError as t:
            if re.search(r'@[A-Za-z]+', code):
                raise Error('Syntax', code, line,
                            'The @ prefix is for referencing the variable, not the variable\'s value.\n'
                            'Use the $ prefix instead')
            raise Error('Type', code, line)
            
        return arg

    def simple(arg, code, line):
        arg = to_arg(arg, code, line)

        if func == do:
            return func(arg, code, line)

        try:
            check_type(sig, arg)
            return func(arg)
        except Msg as m:
            raise Error(m.err, code, line + 1, m.message)
        except TypeError as t:
            if re.search(r'@[A-Za-z][a-z]*', code):
                raise Error('Syntax', code, line + 1,
                            'The @ prefix is for referencing the variable, not the variable\'s value.\n'
                            'Use the $ prefix instead')
            raise Error('Type', code, line + 1)
    
    identity.sig = sig
    simple.sig = sig

    if wrapper == 'id':
        return identity
    if wrapper == 'simple':
        return simple

def reverse(array):
    if hasattr(array, '__iter__'):
        return array[::-1]
    raise Msg('Type', 'Unable to reverse argument: {}'.format(argument))

def decompose(func, arity):
    base = 'lambda x{}: '
    string = ''
    args = []
    for index in range(arity):
        string += base.format(index + 1)
        args.append('x{}'.format(index + 1))
    string += func.__name__ + '(to_arg(' + '), to_arg('.join(args) + ')' * arity
    return eval(string)

def compose(*functions):
    funcs = list(functions)
    def inner(arg):
        ret = arg
        while funcs:
            ret = funcs.pop()(ret)
        return ret
    return inner

def check_type(sig, val):
    if sig == 'Signature -> *Vars -> Block -> *Args -> Any':
        return True
    
    check = types[sig_parse(sig)[0]]
    if callable(check):
        ret = check(val)
    else:
        ret = False
        for option in check:
            if isinstance(val, option):
                ret = True

    if not ret:
        raise Msg('Signature', 'Inconsitent type, {}, for signature: {}'.format(repr(val), sig))

####

def do(iterable_func, line, ln):
    if isinstance(iterable_func, Operator):
        iterable_func = call(iterable_func, line, ln) 
    if isinstance(iterable_func, str) and iterable_func[0] == '$':
        iterable_func = getvar(iterable_func)

    iterable_func(line, ln)
    return iterable_func

def forloop(iters):
    iters = int(iters)
    def variable(var):
        var = var[1:]
        def exe(code):
            def innerdo(line, ln):
                for i in range(1, iters + 1):
                    variables[var] = i
                    evaluate(code, line, ln)
                    
            return innerdo
        return wrappers(exe, 'For[Int][Var]<Block -> Do>')
    return wrappers(variable, 'For[Int]<Var -> Block -> Do>')

def each(array):
    def variable(var):
        var = var[1:]
        def exe(code):
            def innerdo(line, ln):
                for elem in array:
                    variables[var] = elem
                    evaluate(code, line, ln)
                    
            return innerdo
        return wrappers(exe, 'Each[Array][Var]<Block -> Do>')
    return wrappers(variable, 'Each[Array]<Var -> Block -> Do>')

def whileloop(condition):
    def exe(code):
        def innerdo(line, ln):
            while evaluate(condition, line, ln)[-1]:
                evaluate(code, line, ln)

        return innerdo
    return wrappers(exe, 'While[Block]<Block -> Do>')

def dowhile(condition):
    def exe(code):
        def innerdo(line, ln):
            evaluate(code, line, ln)
            while evaluate(condition, line, ln)[-1]:
                evaluate(code, line, ln)

        return innerdo
    return wrappers(exe, 'DoWhile[Block]<Block -> Do>')

class function:
    def __init__(self, sig):
        args = sig_parse(sig)
        if any(i == 'Any' for i in args):
            raise Msg('Signature', 'Any is an invalid function type signature')
        arity = len(args) - 1
        self.sig = sig
        self.signature = args
        self.arity = self.maintain = int(arity)
        self.vars = collections.OrderedDict()
        self.code = None

    def __call__(self, var, line, ln):
        if self.arity == 0:
            self.code = var
        elif self.arity > 0:
            self.vars[var[1:]] = 0
        else:
            ret = self.call(var, line, ln)
            if ret is not None:
                return ret
            
        self.arity -= 1
        return self

    def __repr__(self):
        return 'Function<{}>'.format(' -> '.join(self.signature))

    def call(self, arg, *args):
        update = list(self.vars.keys())[~self.arity]
        self.vars[update] = getvar(arg)

        sig_type = self.signature[~self.arity]
        var_type = getvar(arg)

        try: check_type(sig_type, var_type)
        except Msg as m:
            raise Error(m.err, args[0], args[1], m.message)

        variables.update(dict(self.vars))

        if self.arity == -self.maintain:
            ret = evaluate(self.code, *args)[-1]
            ret_type = self.sig
            
            try: check_type(ret_type, ret)
            except Msg as m:
                raise Error(m.err, args[0], args[1], m.message)
            
            return ret

types = {

    'Int'       : (int,),
    'Real'      : (int, float),
    'Complex'   : (complex),
    'Str'       : (str,),
    'String'    : lambda a: all(type(i)==str for i in a),
    'Array'     : (list,),
    'Dictionary': (dict,),
    'Function'  : callable,
    'Block'     : (Block,),
    'Any'       : lambda a: True,
    'Signature' : lambda v: v != sig_parse(v),
    'Var'       : lambda v: v[0] == '@' and re.search(r'[A-Za-z][a-z]*', v[1:]),
    'Boolean'   : (bool,),
    'Iterable'  : lambda v: hasattr(x, '__iter__'),

}

variables = {}
builtins = {

    # Any ->

    'Print': wrappers(
        print,
        'Print<Any -> Str>',
        'id',
    ),

    'Equal': wrappers(
        lambda x: wrappers(
            lambda y: x == y,
            'Equal[Any]<Any -> Boolean>',
        ),
        'Equal<Any -> Any -> Boolean>',
    ),

    'Unequal': wrappers(
        lambda x: wrappers(
            lambda y: x != y,
            'Unequal[Any]<Any -> Boolean>',
        ),
        'Unequal<Any -> Any -> Boolean>',
    ),

    # Function -> Array -> 

    'Map': wrappers(
        lambda func: wrappers(
            lambda array: list(map(func, array)),
            'Map[Function]<Array -> Array>',
        ),
        'Map<Function -> Array -> Array>',
    ),

    'Filter': wrappers(
        lambda func:  wrappers(
            lambda array: list(filter(func, array)),
            'Filter[Function]<Array -> Array>',
        ),
        'Filter<Function -> Array -> Array>',
    ),

    'Reduce': wrappers(
        lambda func: wrappers(
            lambda array: functools.reduce(func, array),
            'Reduce[Function]<Array -> Real>',
        ),
        'Reduce<Function -> Array -> Real>',
    ),

    # Array ->
    
    'Reverse': wrappers(
        reverse,
        'Reverse<Array -> Array>',
    ),

    'Sort': wrappers(
        sorted,
        'Sort<Array -> Array>',
    ),
    
    'Sum': wrappers(
        sum,
        'Sum<Array -> Real>',
    ),

    'Deque': wrappers(
        lambda array: array[1:],
        'Deque<Array -> Array>',
    ),

    'Pop': wrappers(
        lambda array: array[:-1],
        'Pop<Array -> Array>',
    ),

    # Real ->
    
    'Add': wrappers(
        lambda x: wrappers(
            lambda y: x + y,
            'Add[Real]<Real -> Real>',
        ),
        'Add<Real -> Real -> Real>',
    ),

    'Times': wrappers(
        lambda x: wrappers(
            lambda y: to_arg(x) * to_arg(y),
            'Times[Real]<Real -> Real>',
        ),
        'Times<Real -> Real -> Real>',
    ),

    'Subtract': wrappers(
        lambda x: wrappers(
            lambda y: x - y,
            'Subtract[Real]<Real -> Real>',
        ),
        'Subtract<Real -> Real -> Real>',
    ),

    'Divide': wrappers(
        lambda x: wrappers(
            lambda y: x / y,
            'Divide[Real]<Real -> Real>',
        ),
        'Divide<Real -> Real -> Real>',
    ),

    'Pow': wrappers(
        lambda x: wrappers(
            lambda y: x ** y,
            'Pow[Real]<Real -> Real>',
        ),
        'Pow<Real -> Real -> Real>',
    ),

    'LessThan': wrappers(
        lambda x: wrappers(
            lambda y: x < y,
            'LessThan[Real]<Real -> Boolean>',
        ),
        'LessThan<Real -> Real -> Boolean>',
    ),

    'GreaterThan': wrappers(
        lambda x: wrappers(
            lambda y: x > y,
            'GreaterThan[Real]<Real -> Boolen>',
        ),
        'GreaterThan<Real -> Real -> Boolean>',
    ),

    'LessOrEqualTo': wrappers(
        lambda x: wrappers(
            lambda y: x <= y,
            'LessOrEqualTo[Real]<Real -> Boolean>',
        ),
        'LessOrEqualTo<Real -> Real -> Boolean>',
    ),

    'GreaterOrEqualTo': wrappers(
        lambda x: wrappers(
            lambda y: x >= y,
            'GreaterOrEqualTo[Real]<Real -> Boolean>',
        ),
        'GreaterOrEqualTo<Real -> Real -> Boolean>',
    ),

    # Str ->

    'GetVariable': wrappers(
        lambda var: variables[var],
        'GetVariable<Str -> Any>',
    ),

    'Uppercase': wrappers(
        lambda string: string.upper(),
        'Uppercase<Str -> Str>',
    ),

    'Lowercase': wrappers(
        lambda string: string.lower(),
        'Lowercase<Str -> Str>',
    ),

    # -> Var -> Block -> Do
    
    'For': wrappers(
        forloop,
        'For<Int -> Var -> Block -> Do>',
    ),

    'Each': wrappers(
        each,
        'Each<Array -> Var -> Block -> Do>',
    ),

    'While': wrappers(
        whileloop,
        'While<Block -> Block -> Do>',
    ),

    'DoWhile': wrappers(
        dowhile,
        'DoWhile<Block -> Block -> Do>',
    ),

    # Other
    
    'Do': wrappers(
        do,
        'Do<Function -> Do>',
    ),
    
    'Function': wrappers(
        function,
        'Function<Signature -> *Var -> Block -> *Any -> Any>',
    ),

    # Convert

    'Boolean': wrappers(
        bool,
        'Boolean<Any -> Boolean>',
    ),

    'Integer': wrappers(
        int,
        'Integer<Any -> Int>',
    ),

    'Float': wrappers(
        float,
        'Float<Any -> Real>',
    ),

    'Str': wrappers(
        str,
        'Str<Any -> Str>',
    ),

    'Array': wrappers(
        list,
        'Array<Iterable -> Array>',
    ),

    'Complex': wrappers(
        lambda x: wrappers(
            lambda y: complex(x, y),
            'Complex[Real]<Real -> Complex>',
        ),
        'Complex<Real -> Real -> Complex>',
    ),

}

if __name__ == '__main__':
    try:
        file = open(sys.argv[1])
    except IndexError:
        print('File not provided', file = sys.stderr)
        sys.exit(1)
    except:
        print('Unable to open file: {}'.format(sys.argv[1]), file = sys.stderr)
        sys.exit(1)

    code = file.read()
    file.close()
        
    try:
        execute(code)
    except Error as e:
        print(e, file = sys.stderr)
        sys.exit(1)