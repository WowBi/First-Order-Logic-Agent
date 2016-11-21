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



class Sentence(object):
    # positive = True
    # predicates = []
    def __init__(self, predicates):
        self.predicates = predicates
        # self.positive = positive
        self.type = "Sentence"

    def __repr__(self):
        and_string = ""
        for p in self.predicates:
            and_string = and_string + repr(p) + " & "

        and_string = and_string[:len(and_string) - 3]

        return "{" + and_string + "}"

class CNF(object):
    def __init__(self, clauses):
        self.clauses = clauses
        # self.positive = positive
        self.type = "CNF"

    def __repr__(self):
        result = ""
        for c in self.clauses:
            result = result + repr(c) + " & "
        result = result[: len(result) - 3]
        return "CNF: " + result


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
   'NEGATIVE',
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
# while True:
#     tok = lexer.token()
#     if not tok:
#         break      # No more input
#     print(tok)



#-----------------------------------
def negative_clause_helper(clause):
    if len(clause.predicates) == 1:
        clause.predicates[0].positive = (not clause.predicates[0].positive)
        return clause
    elif len(clause.predicates) > 1:
        new_clauses = []
        for pre in clause.predicates:
            pre.positive = (not pre.positive)
            new_clause = Clause([pre])
            new_clauses.append(new_clause)
        return Sentence(new_clauses)


# def p_cnf_and_cnf(p):
#     '''cnf : LPAREN cnf AND cnf RPAREN'''
#     p[0] = CNF(p[2].clauses + p[4].clauses)
#
#
# def p_sentence_or_sentence(p):
#     '''cnf : LPAREN sentence OR sentence RPAREN'''
#     clauses = []
#     for p1 in p[2].predicates:
#         for p2 in p[4].predicates:
#             clause = Clause([p1, p2])
#             clauses.append(clause)
#     p[0] = CNF(clauses)

# def p_clause_predicate(p):
#     '''clause : predicate'''
#     p[0] = Clause([p[1]])

def p_clauses_to_cnf(p):
    '''cnf : LPAREN clause AND clause RPAREN'''
    p[0] = CNF([p[2], p[4]])



# def p_imply_clause(p):
#     '''clause : LPAREN clause IMPLY clause RPAREN'''
#     p[2].positive = (not p[2].positive)
#     # not right !!! negative clause or clause
#     predicates = p[2].predicates + p[4].predicates
#     p[0] = Clause(predicates)


def p_and_sentences(p):
    '''sentence : LPAREN sentence AND sentence RPAREN'''
    predicates = p[2].predicates + p[4].predicates
    p[0] = Sentence(predicates)


# def p_sentence_predicate(p):
#     '''sentence : predicate'''
#     print('nono')
#     p[0] = Sentence([p[1]])


def p_negative_sentence(p):
    '''clause : LPAREN NEGATIVE sentence RPAREN'''
    predicates = p[3].predicates
    for pre in predicates:
        pre.positive = (not pre.positive)
    p[0] = Clause(predicates)
#
#
def p_negative_clause(p):
    '''sentence : LPAREN NEGATIVE clause RPAREN'''
    print("555555555555")
    predicates = p[3].predicates
    for pre in predicates:
        pre.positive = (not pre.positive)
    p[0] = Sentence(predicates)

#
def p_negative_predicate(p):
    '''predicate : LPAREN NEGATIVE predicate RPAREN'''
    p[3].positive = (not p[3].positive)
    p[0] = p[3]


def p_clause_clauses(p):
    '''clause : LPAREN clause OR clause RPAREN'''
    print("111")
    predicates = p[2].predicates + p[4].predicates
    p[0] = Clause(predicates)

def p_predicates_to_sentence(p):
    '''sentence : LPAREN predicate AND predicate RPAREN'''
    p[0] = Sentence([p[2], p[4]])

def p_clause_predicates(p):
    '''clause : LPAREN predicate OR predicate RPAREN'''
    print("222")
    p[0] = Clause([p[2], p[4]])






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

# test
# s = "Ancestor(Liz,Billy)"
# result = parser.parse(s)
# print(result)


input_path = "./input.txt"
(queries, ori_KB) = parseInputFile(input_path)


KB = {}

for s in ori_KB:
    result = parser.parse(s)
    print(result)

    # clauses = result.clauses
    # for c in clauses:
    #     predicates = c.predicates
    #     # clause is positive, add this clause in KB
    #     if c.positive:
    #         for p in predicates:
    #             if p.name not in KB:
    #                 KB[p.name] = [c]
    #             else:
    #                 KB[p.name].append(c)
    #
    #     # clause is negative, move ~ inward
    #     else:
    #         for p in predicates:
    #             p.positive = (not p.positive)
    #             if p.name not in KB:
    #                 KB[p.name] = [Clause([p])]
    #             else:
    #                 KB[p.name].append(Clause([p]))

print(KB)

