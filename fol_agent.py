#!/usr/bin/python
from collections import deque
import ply.lex as lex
import ply.yacc as yacc
from copy import deepcopy
import time
import re

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
   'NOT',
   'COMMA',
)

# Regular expression rules for simple tokens
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_FACTOR = r'[A-z]+'
t_AND = r'\&'
t_OR = r'\|'
t_IMPLY = r'=>'
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

def p_not_predicate_argument(p):
    '''predicate : NOT FACTOR LPAREN argument RPAREN'''
    p[0] = Predicate(p[2], p[4], False)

def p_arguments_argument(p):
    '''argument : argument COMMA argument'''
    p[0] = p[1] + p[3]


def p_argument_factor(p):
    '''argument : FACTOR'''
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
            sentence = f.readline().replace(" ", "")
            queries.append(sentence)

        num_of_kb = int(f.readline().strip())
        for i in range(num_of_kb):
            sen = f.readline().replace(" ", "")
            knowledge_base.append(sen)
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
                        item2i = deepcopy(item2[i])
                        item2i.append(result[1])
                        value.append(item2i)
                    elif not isinstance(item2[i], list) and isinstance(result[1], list):
                        result1 = deepcopy(result[1])
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
                        item1i = deepcopy(item1[i])
                        item1i.append(result[2])
                        value.append(item1i)
                    elif not isinstance(item1[i], list) and isinstance(result[2], list):
                        result2 = deepcopy(result[2])
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
                        temp = deepcopy(item2)
                        temp.append(item1[i])
                        value.append(temp)
                return value

            elif item1[0] == "|" and item2[0] == "&":
                value = ["&"]
                for i in range(1, len(item2)):
                    if isinstance(item2[i],list):
                        value.append(item2[i] + item1[1:])
                    else:
                        temp = deepcopy(item1)
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

def hasNumbers(inputString):
    return any(char.isdigit() for char in inputString)
# standardize variables of clauses
# standardize clause
def standardize(clause, var_map):
    new_visited_variables = set()
    map = {}
    for predicate in clause.predicates:
        for i in range(len(predicate.arguments)):
            arg = predicate.arguments[i]
            if not arg.islower():
                continue

            if hasNumbers(arg):
                match = re.match(r"([a-z]+)([0-9]+)", arg, re.I)
                if match:
                    items = match.groups()
                    # items is tuple
                    prefix = items[0]
                    if prefix not in var_map:
                        new_visited_variables.add(prefix)
                        continue
                    elif prefix in var_map and prefix in map:
                        predicate.arguments[i] = map[prefix]
                    elif prefix in var_map and prefix not in map:

                        new_var = prefix + str(var_map[prefix])
                        predicate.arguments[i] = new_var
                        map[prefix] = new_var
                        var_map[prefix] = var_map[prefix] + 1
            else:
                if arg not in var_map:
                    new_visited_variables.add(arg)
                    continue
                elif arg in var_map and arg in map:
                    predicate.arguments[i] = map[arg]
                elif arg in var_map and arg not in map:

                    new_var = arg + str(var_map[arg])
                    predicate.arguments[i] = new_var
                    map[arg] = new_var
                    var_map[arg] = var_map[arg] + 1

    for var in new_visited_variables:
        var_map[var] = 1


def addClause2KB(clause, KB):
    for i in range(0, len(clause.predicates)):
        predicate = clause.predicates[i]
        if predicate.name in KB and clause not in KB[predicate.name]:
            KB[predicate.name].append(clause)
        elif predicate.name not in KB:
            KB[predicate.name] = [clause]

def equality_lists(list1, list2):
    if len(list1) != len(list2):
        return False
    for i in range(len(list1)):
        if list1[i] != list2[i]:
            return False
    return True

# return value: (can_unify, substitutions)
def unify(predicate1, predicate2):
    curr_substitutions = {}

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
                else:
                    curr_substitutions[args1[i]] = args2[i]
                # if they both are not in curr_substitutions, leave them alone
        return (True, curr_substitutions)

# return value: add_flag(True or False)
def simplify(clause):
    predicates = clause.predicates
    delete_index = []
    for i in range(len(predicates) - 1):
        for j in range(i + 1, len(predicates)):
            if predicates[i].name == predicates[j].name:
                args1 = predicates[i].arguments
                args2 = predicates[j].arguments
                if len(args1) != len(args2):
                    continue
                if predicates[i].positive == (not predicates[j].positive):
                    (unify_flag, substitutions) = unify(predicates[i], predicates[j])

                    if unify_flag:
                        all_lowercase_values = True
                        for (k, v) in substitutions.items():
                            if not v.islower():
                                all_lowercase_values = False
                        if all_lowercase_values:
                            return False
                else:
                    if equality_lists(args1, args2):
                        delete_index.append(i)
                        continue
                    (unify_flag, substitutions) = unify(predicates[i], predicates[j])
                    if unify_flag:
                        all_lowercase_values = True
                        for (k,v) in substitutions.items():
                            if not v.islower():
                                all_lowercase_values = False
                        if all_lowercase_values:
                            delete_index.append(i)

    delete_index.sort(reverse = True)
    for index in delete_index:
        del predicates[index]
    return True


# return value: (can_resolve, list of new-generated clauses)
def resolve(clause1, clause2, var_map):

    resolvent_s = []
    for i in range(len(clause1.predicates)):
        for j in range(len(clause2.predicates)):

            pre1 = clause1.predicates[i]
            pre2 = clause2.predicates[j]

            if (pre1.name == pre2.name) and (pre1.positive == (not pre2.positive)):
                (can_unify, substitutions) = unify(pre1, pre2)
                if not can_unify:
                    continue

                new_predicates1 = deepcopy(clause1.predicates)
                new_predicates2 = deepcopy(clause2.predicates)

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

                new_clause = Clause(new_predicates1 + new_predicates2)
                # processing new generated clause
                standardize(new_clause, var_map)
                keep_flag = simplify(new_clause)
                if keep_flag:
                    resolvent_s.append(new_clause)

    if len(resolvent_s) == 0:
        return (False, resolvent_s)
    else:
        return (True, resolvent_s)


# list1 and list2 only contains clauses
def belong_to(list1, list2):
    for clause in list1:
        if clause not in list2:
            return False
    return True


def resolution(KB_map, query, var_map):
    start = time.time()
    query.positive = (not query.positive)
    added_clause = Clause([query])

    if query.name not in KB_map:
        KB_map[query.name] = [added_clause]
    elif added_clause not in KB_map[query.name]:
        KB_map[query.name].append(added_clause)
    else:
        return True

    key = query.name
    related_clauses = KB_map[key]
    queue = deque([])
    all_resolvent_s = []

    for clause in related_clauses:
        if clause == added_clause:
            continue
        (can_resolve, resolvent_s) = resolve(added_clause, clause, var_map)
        for clause in resolvent_s:
            if len(clause.predicates) == 0:
                return True
            queue.append(clause)
        all_resolvent_s = all_resolvent_s + resolvent_s

    for item in all_resolvent_s:
        for p in item.predicates:
            if p.name in KB_map and item not in KB_map[p.name]:
                KB_map[p.name].append(item)
            elif p.name not in KB_map:
                KB_map[p.name] = [item]


    while queue:
        now = time.time()
        if now - start > 30:
            return False

        curr_clause = queue.popleft()
        all_new_clauses = []

        for pre in curr_clause.predicates:
            if pre.name not in KB_map:
                continue
            else:
                for c in KB_map[pre.name]:
                    now = time.time()
                    if now - start > 30:
                        return False

                    if c == curr_clause:
                        continue
                    (resolve_flag, new_clauses) = resolve(curr_clause, c, var_map)
                    for cl in new_clauses:
                        if len(cl.predicates) == 0:
                            return True
                        queue.append(cl)
                    all_new_clauses = all_new_clauses + new_clauses
        # add new clauses to KB
        for claus in all_new_clauses:
            now = time.time()
            if now - start > 30:
                return False
            for p in claus.predicates:
                if p.name in KB_map and claus not in KB_map[p.name]:
                    KB_map[p.name].append(claus)
                elif p.name not in KB_map:
                    KB_map[p.name] = [claus]
    return False


# ------------------  main start  --------------------------

input_path = "./input.txt"
output_path = "./output.txt"
(queries, ori_KB) = parseInputFile(input_path)

var_map = {}
KB_map = {}

for s in ori_KB:
    result = parser.parse(s)

    result = eliminate_implication(result)

    result = move_not_inward(result)

    result = distribution_or_over_and(result)

    if isinstance(result, Predicate):
        new_clause = Clause([result])
        standardize(new_clause, var_map)
        if result.name in KB_map and new_clause not in KB_map[result.name]:
            KB_map[result.name].append(new_clause)
        elif result.name not in KB_map:
            KB_map[result.name] = [new_clause]

    elif isinstance(result, list):
        if result[0] == "|":
            new_clause = Clause(result[1:])
            standardize(new_clause, var_map)
            addClause2KB(new_clause, KB_map)

        elif result[0] == "&":
            for i in range(1, len(result)):
                if isinstance(result[i], Predicate):
                    new_clause = Clause([result[i]])
                    standardize(new_clause, var_map)
                    if result[i].name in KB_map and new_clause not in KB_map[result[i].name]:
                        KB_map[result[i].name].append(new_clause)
                    elif result[i].name not in KB_map:
                        KB_map[result[i].name] = [new_clause]
                elif isinstance(result[i], list):
                    new_clause = Clause(result[i][1:])
                    standardize(new_clause, var_map)
                    addClause2KB(new_clause, KB_map)

with open(output_path, 'w') as f:

    for q in queries:
        query = parser.parse(q)
        map = deepcopy(KB_map)
        query_result = resolution(map, query, var_map)

        if query_result:
            print_result = "TRUE"
        else:
            print_result = "FALSE"

        f.write(print_result + '\n')





