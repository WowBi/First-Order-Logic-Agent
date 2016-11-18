import ply.lex as lex
import ply.yacc as yacc


class Predicate(object):
    # positive = True
    # arguments = []
    # name = ""
    def __init__(self, name, arguments, positive):
        self.arguments = arguments
        self.name = name
        self.positive = positive

predicate_map = {}

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
        # print queries
        # print knowledge_base


input_path = "./input.txt"
parseInputFile(input_path)

tokens = (
   'LPAREN',
   'RPAREN',
   'PREDICATE',
   'AND',
   'OR',
   'IMPLY',
   'VAR',
   'NEGATIVE',
   'COMMA',
)

# Regular expression rules for simple tokens
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_PREDICATE = r'[A-Z][a-z]*'
t_AND = r'\^'
t_OR = r'\|'
t_IMPLY = r'=>'
t_VAR = r'[a-z]'
t_NEGATIVE = r'~'
t_COMMA = r','
# t_CONSTANT = r'[A-Z][a-z]*'

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
lexer.input("Ancestor(Liz,Bob)")
# while True:
#     tok = lexer.token()
#     if not tok:
#         break      # No more input
#     print(tok)


# def p_sentence_imply(p):
#     'sentence : clause IMPLY clause'
#     p[0] = ('IMPLY', p[1], p[3])
#
# def p_sentence_and(p):
#     'sentence : clause AND clause'
#     p[0] = ('AND', p[1], p[3])
#
# def p_sentence_or(p):
#     'sentence : clause OR clause'
#     p[0] = ('OR', p[1], p[3])
#
# def p_clause_negative(p):
#     'clause : NEGATIVE clause'
#     p[0] = ('NEGATIVE', p[2])
#
# def p_predicate(p):
#     'clause: PREDICATE LPAREN argument RPAREN'
#     p[0] = ('PREDICATE', p[1], p[3])
def p_negative_clause(p):
    '''clause : NEGATIVE clause'''
    result = []
    if isinstance(p[1], list):
        # ['PREDICATE', 'H', ['x']]
        if p[1][0] == "PREDICATE":
            p[1][0] = "NEGATIVE_PREDICATE"
            result = p[1]
        # # ['OR', A, B, C]
        # if len(p[1]) >= 3:
        #     result.append("AND")
        #     for x in range(1, len(p[1])):
        #         result.append(["NEGATIVE", p[1][x]])
        # # ['NEGATIVE', A]
        # if len(p[1]) == 2:
        #     result.append(p[1][1])
        # if len(p[1]) == 1:
        #     result.append(p[1][0])
    else:
        result.append(["NEGATIVE", p[1]])

    p[0] = result

def p_imply(p):
    '''clause : LPAREN predicate IMPLY predicate RPAREN'''
    negative_part = []
    if isinstance(p[2], list):
        if p[2][0] == "PREDICATE":
            p[2][0] = "NEGATIVE_PREDICATE"
            negative_part = p[2]

    p[0] = ['OR', negative_part, p[4]]


def p_clause_predicates(p):
    '''clause : predicate'''
    p[0] = p[1]

def p_predicates(p):
    '''predicate : predicate OR predicate'''
    result = ["OR"]
    if isinstance(p[1], list):
        for x in range(1, len(p[1])):
            result.append(p[1][x])
    else:
        result.append(p[1])

    if isinstance(p[3], list):
        for x in range(1, len(p[3])):
            result.append(p[3][x])
    else:
        result.append(p[3])

    p[0] = result


def p_predicate_argument(p):
    '''predicate : PREDICATE LPAREN argument RPAREN'''

    p[0] = ['PREDICATE', p[1], p[3]]

def p_argument_var(p):
    '''argument : VAR'''
    p[0] = p[1]

def p_arguments_argument(p):
    '''argument : argument COMMA argument'''
    result = [","]
    if isinstance(p[1], list):
        for x in range(1, len(p[1])):
            result.append(p[1][x])
    else:
        result.append(p[1])

    if isinstance(p[3], list):
        for x in range(1, len(p[3])):
            result.append(p[3][x])
    else:
        result.append(p[3])

    p[0] = result


# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")


# precedence = (
#     ('AND', 'IMPLY'),
#     ('LPAREN', 'RPAREN'),
#     ('NEGATIVE'),
#     )

# Build the parser
parser = yacc.yacc()

KB = []
KB.append('(A(x) => H(x, y, z))')
# KB.append('A(x) | ~H(x)')
# KB.append('A(x) => H(x)')
# KB.append('Mother(Liz,Charley)')



for s in KB:
    result = parser.parse(s)
    print(result)
