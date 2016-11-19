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
        return self.name + "(" + ",".join(self.arguments) + ")"

    def __str__(self):
        if self.positive:
            return self.name + "(" + ",".join(self.arguments) + ")"
        else:
            return "~" + self.name + "(" + ",".join(self.arguments) + ")"

class Clause(object):
    # positive = True
    # predicates = []
    def __init__(self, predicates, positive = True):
        self.predicates = predicates
        self.positive = positive
        self.type = "Clause"

    def __repr__(self):
        or_string = ""
        for p in self.predicates:
            or_string = or_string + str(p) + " | "
        or_string = or_string[:len(or_string) - 3]
        if self.positive:
            return "(" + or_string + ")"
        else:
            return "~(" + or_string + ")"

    def __str__(self):
        or_string = ""
        for p in self.predicates:
            or_string = or_string + str(p) + " | "
        or_string = or_string[:len(or_string) - 3]
        if self.positive:
            return "(" + or_string + ")"
        else:
            return "~(" + or_string + ")"

class Sentence(object):
    # positive = True
    # clauses = []
    def __init__(self, clauses, positive = True):
        self.clauses = clauses
        self.positive = positive
        self.type = "Sentence"

    def __repr__(self):
        and_string = ""
        for c in self.clauses:
            or_string = ""
            if len(c.predicates) == 1:
                or_string = str(c.predicates[0])
            elif len(c.predicates) > 1:
                for p in c.predicates:
                    or_string = or_string + str(p) + " | "
                or_string = or_string[:len(or_string) - 3]

            and_string = and_string + or_string + " & "
        and_string = and_string[:len(and_string) - 3]

        if self.positive:
            return "(" + and_string + ")"
        else:
            return "~(" + and_string + ")"


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

# set up tokens
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
t_AND = r'\&'
t_OR = r'\|'
t_IMPLY = r'=>'
t_VAR = r'[a-z]'
t_NEGATIVE = r'~'
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
lexer.input("(A(x)&H(x, y))")
# while True:
#     tok = lexer.token()
#     if not tok:
#         break      # No more input
#     print(tok)



# def negative_clause_helper(p):
#     if isinstance(p, list):
#         # ['PREDICATE', 'H', ['x']]
#         if p[0] == "PREDICATE":
#             p[0] = "NEGATIVE_PREDICATE"
#
#         if p[0] == "OR":
#             p[0] = "AND"
#             for i in range(1, len(p)):
#                 negative_clause_helper(p[i])
#
#         if p[0] == "AND":
#             p[0] = "OR"
#             for i in range(1, len(p)):
#                 negative_clause_helper(p[i])
#         return p
#
#     else:
#         return ["NEGATIVE", p]

# def p_negative_sentence(p):
#     '''clause : NEGATIVE sentence'''
#     predicates = p[2][1:]
#     for pre in predicates:
#         pre.positive = (not pre.positive)
#     p[0] = Clause(predicates)

# def p_negative_clause(p):
#     '''sentence : NEGATIVE clause'''
#     predicates = p[2].predicates
#     for pre in predicates:
#         pre.positive = (not pre.positive)
#     p[0] = ["AND"] + predicates


#-----------------------------------

def p_negative_sentence(p):
    '''sentence : NEGATIVE sentence'''
    p[2].positive = (not p[2].positive)
    p[0] = p[2]


# def p_negative_clauses(p):
#     '''sentence : NEGATIVE clause'''
#     predicates = p[2].predicates
#     clauses = []
#     for pre in predicates:
#         pre.positive = (not pre.positive)
#         clauses.append(Clause([pre]))
#     p[0] = Sentence(clauses)


def p_negative_clauses(p):
    '''clause : NEGATIVE clause'''
    p[2].positive = (not p[2].positive)
    p[0] = p[2]


def p_imply_clause(p):
    '''clause : LPAREN clause IMPLY clause RPAREN'''
    for pre in p[2].predicates:
        pre.positive = (not pre.positive)
    predicates = p[2].predicates + p[4].predicates
    p[0] = Clause(predicates)


def p_clause_predicates(p):
    '''clause : LPAREN clause OR clause RPAREN'''
    predicates = p[2].predicates + p[4].predicates
    p[0] = Clause(predicates)

def p_and_sentences(p):
    '''sentence : LPAREN sentence AND sentence RPAREN'''
    clauses = p[2].clauses + p[4].clauses
    p[0] = Sentence(clauses)

def p_sentence_clauses(p):
    '''sentence : LPAREN clause AND clause RPAREN'''
    p[0] = Sentence([p[2], p[4]])

# All types are changed to sentence, so when changing sentence does not need positive to mark
# but we need positive flag when doing negative operation
def p_sentence_clause(p):
    '''sentence : clause'''
    positive = p[1].positive
    p[1].positive = (not p[1].positive)
    p[0] = Sentence([p[1]], positive)

def p_clause_predicate(p):
    '''clause : predicate'''
    # it seems negative clause rule did negating
    p[0] = Clause([p[1]])



# -------- with no parenthesis parsing --------

# def p_clause_predicates(p):
#     '''clause : clause OR clause'''
#     predicates = p[1].predicates + p[3].predicates
#     p[0] = Clause(predicates)
#
# def p_sentence_predicates(p):
#     '''sentence : clause AND clause'''
#     p[0] = Sentence([p[1], p[3]])
#
# def p_clause_parenthesis(p):
#     '''clause : LRAREN clause RPAREN'''
#     p[0] = p[2]
#
# def p_sentence_parenthesis(p):
#     '''sentence : LRAREN sentence RPAREN'''
#     p[0] = p[2]




def p_predicate_argument(p):
    '''predicate : PREDICATE LPAREN argument RPAREN'''
    p[0] = Predicate(p[1], p[3])


def p_argument_var(p):
    '''argument : VAR'''
    p[0] = [p[1]]

def p_arguments_argument(p):
    '''argument : argument COMMA argument'''
    p[0] = p[1] + p[3]



# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")


# precedence = (
#     ('AND', 'IMPLY'),
#     ('left', 'LPAREN', 'RPAREN'),
#     ('NEGATIVE'),
# )

# Build the parser
parser = yacc.yacc()

KB = []
# KB.append('(A(x, y)| H(x, y, z))')
# KB.append('~B(x, y)')

KB.append('((A(y) & H(x)) & B(z))')
KB.append('(A(y) | H(x))')
KB.append('(A(y) => H(x))')
KB.append('~(A(x) | B(y))')
KB.append('C(x)')
for s in KB:
    result = parser.parse(s)
    print(result)

