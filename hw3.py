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

        return "clause: (" + or_string + ")"

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




def isClause(q):
    if not isinstance(q, list):
        return True
    elif q[0] != "|":
        return False
    else:
        for i in range(1, len(q)):
            if isinstance(q[i], list):
                return False
        return True

# i.e. CNF
def isSentence(q):
    if not isinstance(q, list):
        return True
    elif q[0] != "&":
        return False
    else:
        for i in range(1, len(q)):
            if not isClause(q[i]):
                return False
        return True

def negate_item(item):
    if isClause(item):
        if isinstance(item, list):
            item[0] = "&"
            for i in range(1, len(item)):
                item[i].positive = (not item[i].positive)
        else:
            item.positive = (not item.positive)
        return item
    else:
        if item[0] == "~":
            item = item[1]
            # not OK in [&, A, [~, B]]
            while isinstance(item, list) and item[0] == "~":
                item = negate_item(item)
            return item
        if item[0] == "&":
            item[0] = "|"
            item1 = negate_item(item[1])
            item2 = negate_item(item[2])
            return ["|", item1, item2]

        if item[0] == "|":
            item[0] = "&"
            item1 = negate_item(item[1])
            item2 = negate_item(item[2])
            return ["&", item1, item2]

        if item[0] == "=>":
            item[0] = "&"
            item2 = negate_item(item[2])
            return ["&", item[1], item2]

# step 1: eliminate implication
def eliminate_implication(result):
    if isClause(result):
        return result
    elif result[0] == "=>":
        item1 = eliminate_implication(result[1])
        item1 = negate_item(item1)
        item2 = eliminate_implication(result[2])
        return ["|", item1, item2]
    else:
        for i in range(1, len(result)):
            result[i] = eliminate_implication(result[i])
        return result


# step 2: Move NOT inward
def move_not_inward(result):
    if isClause(result):
        return result
    elif result[0] == "~":
        return negate_item(result[1])
    else:
        for i in range(1, len(result)):
            result[i] = move_not_inward(result[i])
        return result


# step 3: Distribution OR over AND
def distribution_or_over_and(result):
    if isClause(result) or isSentence(result):
        return result

    if result[0] == "|":
        if isClause(result[1]) and isClause(result[2]):
            value = ["|"]

            if isinstance(result[1], list):
                value = value + result[1][1:]
            else:
                value.append(result[1])

            if isinstance(result[2], list):
                value = value + result[2][1:]
            else:
                value.append(result[2])

            return value

        elif isClause(result[1]):
            item2 = distribution_or_over_and(result[2])
            if item2[0] == "|":
                if isinstance(result[1], list):
                    item2 = item2 + result[1][1:]
                else:
                    item2.append(result[1])
                return item2

            if item2[0] == "&":
                value = ["&"]
                for i in range(1, len(item2)):
                    if isinstance(item2[i], list) and isinstance(result[1], list):
                        value.append(item2[i] + result[1][1:])
                    elif isinstance(item2[i], list) and not isinstance(result[1], list):
                        value.append(item2[i].append(result[1]))
                    elif not isinstance(item2[i], list) and isinstance(result[1], list):
                        value.append(result[1].append(item2[i]))
                    else:
                        value.append(["|", item2[i], result[1]])
                return value

        elif isClause(result[2]):
            item1 = distribution_or_over_and(result[1])
            if item1[0] == "|":
                if isinstance(result[2], list):
                    item1 = item1 + result[2][1:]
                else:
                    item1.append(result[2])
                return item1

            if item1[0] == "&":
                value = ["&"]
                for i in range(1, len(item1)):
                    if isinstance(item1[i], list) and isinstance(result[2], list):
                        value.append(item1[i] + result[2][1:])
                    elif isinstance(item1[i], list) and not isinstance(result[2], list):
                        value.append(item1[i].append(result[2]))
                    elif not isinstance(item1[i], list) and isinstance(result[2], list):
                        value.append(result[2].append(item1[i]))
                    else:
                        value.append(["|", item1[i], result[2]])
                return value
        else:
            item1 = distribution_or_over_and(result[1])
            item2 = distribution_or_over_and(result[2])
            # item[0] == "|", item is a clause
            # item[0] == "&", item is a sentence
            if item1[0] == "|" and item2[0] == "|":
                return item1 + item2[1:]
            elif item1[0] == "&" and item2[0] == "&":
                return item1 + item2[1:]
            elif item1[0] == "&" and item2[0] == "|":
                value = ["&"]
                for i in range(1, len(item1)):
                    if isinstance(item1[i], list):
                        value.append(item1[i] + item2[1:])
                    else:
                        value.append(item2.append(item1[i]))

            elif item1[0] == "|" and item2[0] == "&":
                value = ["&"]
                for i in range(1, len(item2)):
                    if isinstance(item2[i],list):
                        value.append(item2[i] + item1[1:])
                    else:
                        value.append(item1.append(item2[i]))
    if result[0] == "&":
        if isClause(result[1]) and isClause(result[2]):
            return result

        elif isClause(result[1]):
            item2 = distribution_or_over_and(result[2])
            if item2[0] == "|":
                return ["&", result[1], item2]

            if item2[0] == "&":
                value = ["&", result[1]]
                for i in range(1, len(item2)):
                    value.append(item2[i])
                return value

        elif isClause(result[2]):
            item1 = distribution_or_over_and(result[1])
            if item1[0] == "|":
                return ["&", result[2], item1]

            if item1[0] == "&":
                value = ["&", result[2]]
                for i in range(1, len(item1)):
                    value.append(item1[i])
                return value

        elif not isClause(result[1]) and not isClause(result[2]):
            item1 = distribution_or_over_and(result[1])
            item2 = distribution_or_over_and(result[2])
            if item1[0] == "|" and item2[0] == "|":
                return ["&", item1, item2]

            elif item1[0] == "&" and item2[0] == "&":
                value = ["&"]
                for i in range(1, len(item1)):
                    value.append(item1[i])
                for i in range(1, len(item2)):
                    value.append(item2[i])
                return value

            elif item1[0] == "&" and item2[0] == "|":
                value = ["&", item2]
                for i in range(1, len(item1)):
                    value.append(item1[i])
                return value

            elif item1[0] == "|" and item2[0] == "&":
                value = ["&", item1]
                for i in range(1, len(item2)):
                    value.append(item2[i])
                return value


def addClauseToKB(clauseList, KB):
    new_clause = Clause(clauseList[1:])
    for i in range(1, len(clauseList)):
        predicate = clauseList[i]
        if predicate.name in KB:
            KB[predicate.name].append(new_clause)
        else:
            KB[predicate.name] = [new_clause]



input_path = "./input.txt"
(queries, ori_KB) = parseInputFile(input_path)


KB = {}

for s in ori_KB:
    result = parser.parse(s)
    print(result)
    result = eliminate_implication(result)
    print("after elimination implication")
    print(result)
    result = move_not_inward(result)
    print("after move ~ inwards")
    print(result)
    result = distribution_or_over_and(result)
    print("after distribution")
    print(result)
    print("\n")

    if isinstance(result, Predicate):
        KB[result.name] = Clause([result])
    elif isinstance(result, list):
        if result[0] == "|":
            addClauseToKB(result, KB)
        elif result[0] == "&":
            for i in range(1, len(result)):
                if isinstance(result[i], Predicate):
                    new_clause = Clause([result[i]])
                    if result[i].name in KB:
                        KB[result[i].name].append(new_clause)
                    else:
                        KB[result[i].name] = [new_clause]
                elif isinstance(result[i], list):
                    addClauseToKB(result[i], KB)

print(KB)
print(KB["A"])
print(len(KB["A"]))





