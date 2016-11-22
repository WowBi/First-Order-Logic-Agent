import random
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

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        else:
            return False

    def __repr__(self):
        if self.positive:
            return self.name + "(" + ",".join(self.arguments) + ")"
        else:
            return "~" + self.name + "(" + ",".join(self.arguments) + ")"

class Clause(object):
    # predicates = []
    def __init__(self, predicates):
        self.predicates = predicates
        self.type = "Clause"

    def __eq__(self, other):
        if not isinstance(other, self.__class__ ):
            return False
        else:
            return self.__dict__ == other.__dict__

    # def __ne__(self, other):
    #     return not self.__eq__(other)

    def __repr__(self):
        or_string = ""
        for p in self.predicates:
            or_string = or_string + repr(p) + " | "
        or_string = or_string[:len(or_string) - 3]

        return "clause: (" + or_string + ")\n"

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

# # A regular expression rule with some action code

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
                        item2i = list(item2[i])
                        item2i.append(result[1])
                        value.append(item2i)
                    elif not isinstance(item2[i], list) and isinstance(result[1], list):
                        result1 = list(result[1])
                        result1.append(item2[i])
                        value.append(result1)
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
                        item1i = list(item1[i])
                        item1i.append(result[2])
                        value.append(item1i)
                    elif not isinstance(item1[i], list) and isinstance(result[2], list):
                        result2 = list(result[2])
                        result2.append(item1[i])
                        value.append(result2)
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
                        temp = list(item2)
                        temp.append(item1[i])
                        value.append(temp)
                return value

            elif item1[0] == "|" and item2[0] == "&":
                value = ["&"]
                for i in range(1, len(item2)):
                    if isinstance(item2[i],list):
                        value.append(item2[i] + item1[1:])
                    else:
                        temp = list(item1)
                        temp.append(item2[i])
                        value.append(temp)
                return value

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

# generate a random variable with two letters
def var_gen():
    var_list = [random.choice("abcdefghijklmnopqrstuvwxyz") for i in range(2)]
    return ("".join(var_list))


# standardize variables of clauses
# standardize clause
def standardize(clause, var_set):
    new_visited_variables = set()
    map = {}
    for predicate in clause.predicates:
        for i in range(len(predicate.arguments)):

            if not predicate.arguments[i].islower():
                continue

            if predicate.arguments[i] not in var_set:
                new_visited_variables.add(predicate.arguments[i])
                continue
            elif predicate.arguments[i] in var_set and predicate.arguments[i] in map:
                predicate.arguments[i] = map[predicate.arguments[i]]
            elif predicate.arguments[i] in var_set and predicate.arguments[i] not in map:
                original_var = predicate.arguments[i]
                new_var_found = False
                for num in range(0, 26):
                    new_var = chr(97 + num)
                    if new_var in var_set:
                        continue
                    else:
                        predicate.arguments[i] = new_var
                        map[original_var] = new_var
                        var_set.add(new_var)
                        new_var_found = True
                        break
                while not new_var_found:
                    new_var = var_gen()
                    if new_var in var_set:
                        continue
                    else:
                        predicate.arguments[i] = new_var
                        map[original_var] = new_var
                        var_set.add(new_var)
                        new_var_found = True
    for var in new_visited_variables:
        var_set.add(var)


def addClause2KB(clause, KB):
    for i in range(0, len(clause.predicates)):
        predicate = clause.predicates[i]
        if predicate.name in KB:
            KB[predicate.name].append(clause)
        else:
            KB[predicate.name] = [clause]

def equality_lists(list1, list2):
    if len(list1) != len(list2):
        return False
    for item in list1:
        if item not in list2:
            return False
    return True

def unify(predicate1, predicate2):
    # curr_substitutions = dict((k, v) for k, v in substitutions.items())
    curr_substitutions = {}
    if (predicate1.name == predicate2.name) and (predicate1.positive == (not predicate2.positive)):
        args1 = predicate1.arguments
        args2 = predicate2.arguments
        if equality_lists(args1, args2):
            return (True, {})
        else:
            for i in range(len(args1)):
                if args1[i] == args2[i]:
                    continue
                elif not args1[i].islower() and not args2[i].islower():
                    return (False, {})

                elif args1[i].islower() and not args2[i].islower():
                    if args1[i] in curr_substitutions and curr_substitutions[args1[i]] != args2[i]:
                        return (False, {})
                    curr_substitutions[args1[i]] = args2[i]

                elif not args1[i].islower() and args2[i].islower():
                    if args2[i] in curr_substitutions and curr_substitutions[args2[i]] != args1[i]:
                        return (False, {})
                    curr_substitutions[args2[i]] = args1[i]

                elif args1[i].islower() and args2[i].islower():
                    if args1[i] in curr_substitutions and args2[i] in curr_substitutions:
                        if curr_substitutions[args1[i]] != curr_substitutions[args2[i]]:
                            return (False, {})
                    elif args1[i] in curr_substitutions:
                        curr_substitutions[args2[i]] = curr_substitutions[args1[i]]
                    elif args2[i] in curr_substitutions:
                        curr_substitutions[args1[i]] = curr_substitutions[args2[i]]
                    # if they both are not in curr_substitutions, leave them alone
            return (True, curr_substitutions)
    else:
        return (False, {})


def resolve(clause1, clause2):
    if len(clause1.predicates) == 1 and len(clause2.predicates) == 1:
        (success, substitutions) = unify(clause1.predicates[0], clause2.predicates[0])
        if not success:
            return (False, [])
        elif success and not substitutions:
            # contradiciton flag, and substitution
            return (True, []) # contradiction
        elif success and substitutions:
            return (False, [])

    elif len(clause1.predicates) == 1 and len(clause2.predicates) > 1:
        resolvent_s = []
        for i in range(len(clause2.predicates)):
            pre = clause2.predicates[i]
            (success, substitutions) = unify(clause1.predicates[0], pre)
            if success and not substitutions:
                return (True, [])
            elif success and substitutions:
                new_predicates = list(clause2.predicates)
                del new_predicates[i]
                for p in new_predicates:
                    for index in range(len(p.arguments)):
                        if p.arguments[index] in substitutions:
                            p.arguments[index] = substitutions[p.arguments[index]]
                resolvent_s.append(Clause(new_predicates))
        return [False, resolvent_s]

    elif len(clause2.predicates) == 1 and len(clause1.predicates) > 1:
        resolvent_s = []
        for i in range(len(clause1.predicates)):
            pre = clause1.predicates[i]
            (success, substitutions) = unify(clause2.predicates[0], pre)
            if success and not substitutions:
                return (True, [])
            elif success and substitutions:
                new_predicates = list(clause1.predicates)
                del new_predicates[i]
                for p in new_predicates:
                    for index in range(len(p.arguments)):
                        if p.arguments[index] in substitutions:
                            p.arguments[index] = substitutions[p.arguments[index]]
                resolvent_s.append(Clause(new_predicates))
        return [False, resolvent_s]


    elif len(clause2.predicates) > 1 and len(clause1.predicates) > 1:
        resolvent_s = []
        for i in range(len(clause1.predicates)):
            for j in range(len(clause2.predicates)):
                pre1 = clause1.predicates[i]
                pre2 = clause2.predicates[j]
                (success, substitutions) = unify(pre1, pre2)
                if success and not substitutions:
                    return (True, [])
                elif success and substitutions:
                    new_predicates1 = list(clause1.predicates)
                    new_predicates2 = list(clause2.predicates)
                    del new_predicates1[i]
                    del new_predicates2[j]
                    for p in new_predicates1:
                        for index in range(len(p.arguments)):
                            if p.arguments[index] in substitutions:
                                p.arguments[index] = substitutions[p.arguments[index]]
                    for p in new_predicates2:
                        for index in range(len(p.arguments)):
                            if p.arguments[index] in substitutions:
                                p.arguments[index] = substitutions[p.arguments[index]]
                    resolvent_s.append(Clause(new_predicates1 + new_predicates2))
        return [False, resolvent_s]



# list1 and list2 only contains clauses
def belong_to(list1, list2):
    for clause in list1:
        if clause not in list2:
            return False
    return True

# a simple resolution algorithm for First Order Logic
# query is a predicate
# may need remove new added clause after this process
def fol_resolution(KB_clauses, query):
    query.positive = (not query.positive)
    added_clause = Clause([query])
    KB_clauses.append(added_clause)
    new_clauses = []
    while True:
        for i in range(len(KB_clauses)):
            for j in range(i + 1, len(KB_clauses)):
                (contradiction, resolvent_s) = resolve(KB_clauses[i], KB_clauses[j])
                # resolvent_s are new generated clauses
                if contradiction:
                    return True
                for c in resolvent_s:
                    if c not in new_clauses:
                        new_clauses.append(c)
        if belong_to(new_clauses, KB_clauses):
            return False
        KB_clauses = list(set(KB_clauses + new_clauses))


input_path = "./test1.txt"
(queries, ori_KB) = parseInputFile(input_path)

var_set = set()

KB_map = {}
KB_clauses = list()

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
    # print("\n")

    if isinstance(result, Predicate):
        new_clause = Clause([result])
        standardize(new_clause, var_set)
        # add new clauses to set list
        KB_clauses.append(new_clause)
        # print(new_clause)
        if result.name in KB_map:
            KB_map[result.name].append(new_clause)
        else:
            KB_map[result.name] = [new_clause]
    elif isinstance(result, list):
        if result[0] == "|":
            new_clause = Clause(result[1:])
            standardize(new_clause, var_set)
            # add new clauses to set list
            KB_clauses.append(new_clause)
            # print(new_clause)
            addClause2KB(new_clause, KB_map)
        elif result[0] == "&":
            for i in range(1, len(result)):
                if isinstance(result[i], Predicate):
                    new_clause = Clause([result[i]])
                    standardize(new_clause, var_set)
                    # add new clauses to set list
                    KB_clauses.append(new_clause)
                    # print(new_clause)
                    if result[i].name in KB_map:
                        KB_map[result[i].name].append(new_clause)
                    else:
                        KB_map[result[i].name] = [new_clause]
                elif isinstance(result[i], list):
                    new_clause = Clause(result[i][1:])
                    standardize(new_clause, var_set)
                    # add new clauses to set list
                    KB_clauses.append(new_clause)
                    # print(new_clause)
                    addClause2KB(new_clause, KB_map)
    print("\n")



print("My solutions: ")
for q in queries:
    query = parser.parse(q)
    query_result = fol_resolution(KB_clauses, query)
    print(query_result)

# print(var_set)
# print(KB_clauses)

# a = Predicate("yes", [1,2,3], True)
# b = Predicate("yes", [1,2,3], True)
# c = Predicate("no", ["x"])
# print(a == b)
#
# a_clause = Clause([a, c])
# b_clause = Clause([b, c])
# print(a_clause == b_clause)
#
# test = [a_clause, b_clause]
# print(a_clause in test)




