import ply.lex as lex
import ply.yacc as yacc


class Predicate(object):
    # positive = True
    # arguments = []
    # name = ""
    def __init__(self, name, arguments, positive = True):
        self.arguments = arguments
        self.name = name
        self.positive = positive
        self.type = "Predicate"

    def __repr__(self):
        if self.positive:
            return self.name + "(" + ",".join(self.arguments) + ")"
        else:
            return "~" + self.name + "(" + ",".join(self.arguments) + ")"

class Clause(object):
    # positive = True
    # predicates = []
    def __init__(self, predicates):
        self.predicates = predicates
        # self.positive = positive
        self.type = "Clause"

    def __repr__(self):
        or_string = ""
        for p in self.predicates:
            or_string = or_string + repr(p) + " | "
        or_string = or_string[:len(or_string) - 3]

        return "(" + or_string + ")"

def parseInputFile(input_path):

    with open(input_path, 'r') as f:
        queries = []
        knowledge_base = []
        num_of_queries = int(f.readline().strip())
        for i in range(num_of_queries):
            queries.append(f.readline().strip())

        num_of_kb = int(f.readline().strip())
        for i in range(num_of_kb):
            knowledge_base.append(f.readline().strip())
        return (queries, knowledge_base)

# set up tokens
tokens = (
   'LPAREN',
   'RPAREN',
   'FACTOR', # predicates and constants
   'AND',
   'OR',
   'IMPLY',
   'VAR',
   'NOT',
   'COMMA',
)

# Regular expression rules for simple tokens
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_FACTOR = r'[A-Z][a-z]*'
t_AND = r'\&'
t_OR = r'\|'
t_IMPLY = r'=>'
t_VAR = r'[a-z]'
t_NOT = r'~'
t_COMMA = r','

# A regular expression rule with some action code
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()




def p_not_expression(p):
    '''expression : LPAREN NOT expression RPAREN'''
    if not isinstance(p[3], list):
        p[3].positive = (not p[3].positive)
        p[0] = p[3]
    else:
        p[0] = ["~", p[3]]

def p_imply(p):
    '''expression : LPAREN expression IMPLY expression RPAREN'''
    p[0] = ["=>", p[2], p[4]]

def p_expression_predicates_and(p):
    '''expression : LPAREN expression AND expression RPAREN'''
    p[0] = ["&", p[2], p[4]]

def p_expression_predicates_or(p):
    '''expression : LPAREN expression OR expression RPAREN'''
    p[0] = ["|", p[2], p[4]]

def p_expression_predicate(p):
    '''expression : predicate'''
    p[0] = p[1]

def p_predicate_argument(p):
    '''predicate : FACTOR LPAREN argument RPAREN'''
    p[0] = Predicate(p[1], p[3])


def p_arguments_argument(p):
    '''argument : argument COMMA argument'''
    p[0] = p[1] + p[3]


def p_argument_factor(p):
    '''argument : FACTOR
                | VAR'''
    p[0] = [p[1]]

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")

# Build the parser
parser = yacc.yacc()

input_path = "./input.txt"
(queries, ori_KB) = parseInputFile(input_path)


KB = {}

for s in ori_KB:
    result = parser.parse(s)

print(KB)


