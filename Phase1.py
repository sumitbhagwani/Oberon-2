##########################################################################################
#Symbol Table
###########################################################################################
#from symtab2 import varEntry

userChoice1 = raw_input("Do you wish to print the Intermediate code? y/n: ")

if(userChoice1=="y"):
	debug3 = True;
else:
	debug3 = False;

userChoice2 = raw_input("Do you wish to print the other debugging information? y/n: ")
if(userChoice2=="y"):
	debug2 = True;
	debug4 = True
else:
	debug2 = False;
	debug4 = False


# Debugging Flags :
flag = True;
flag2 = True;

tokenPrinting = False;

tempCount = 0
labelCount = 0

quadList = []

targetList = [];
constList = []
stringList = [];
def emit(q):
    global quadList;
    global debug4
    if(debug4):
        print len(quadList);
        q.Print();
    quadList.append(q);
    
def nextQuad():
    global quadList;
    return len(quadList);

def makeList(l):
    return [l];

def makeList_empty():
    return ([])

def merge(l1,l2):
    l = l1[:];
    l.extend(l2);
    return l;

def backPatch(l,q):
    global quadList;
    global targetList;
    global debug4
    targetList.append(q);
    if(debug4):
        if(len(l)>0):
            print "backpatching! "
        else:
            print "No backpatching done!"
    for each in l:
        if(debug4):
            quadList[each].Print()
        quadList[each].result = q;
        if(debug4):
            quadList[each].Print()

#intSize = 2
intSize = 4
realSize = 4
charSize = 4
booleanSize = 4
pointerSize = 0


def newTemp(type):
    global tempCount
    global debug4
    if(debug4):
        print "hello"
    tempCount+=1
    
    toReturn = "t"+str(tempCount)
    
    if(type=='INTEGER'):
        typeSize = intSize
    elif(type=='REAL'):
        typeSize = realSize
    elif(type=='BOOLEAN'):
        typeSize = booleanSize
    elif(type=='CHAR'):
        typeSize = charSize
    elif(type=='VOID'):
        return toReturn 
    
    
    global stack
    symbolTable = stack[len(stack)-1]
    
    currentOffset = symbolTable.offset
                            
    symbolTable.offset = symbolTable.offset + typeSize
        
    #print "creating symbol table entry for ", toReturn," ", type," ",currentOffset
    entry = VarEntry(toReturn, type, currentOffset)
    symbolTable.Insert(entry)
    
    return toReturn

def newLabel():
    global labelCount
    labelCount+=1
    return "label"+str(labelCount)+":"

unaryOps = ['UMINUS' , 'UPLUS' , '~' , 'intToReal'];
relOps = ['<' , '>' , '=' , '#' , '<=' , '>='];
        
class Quad():
    def __init__(self, op = None , arg1 = None, arg2 = None, result = None, opType = None):
        self.op = op
        self.arg1 = arg1
        self.arg2 = arg2
        self.result = result
        self.opType = opType
        
        
    def Print(self, num = -1):
        #print "op " , self.op;
        #print "arg1 " , self.arg1;
        #print "arg2 " , self.arg2;
        #print "result " , self.result;
        #print "\n"
        global unaryOps;
        global relOps;
        global debug4
        if(self.op=='null'):
          if(debug4):
              print num,": Do nothing!";  
        elif(self.op == 'goto'):
            print num,":", self.op , self.result;
        elif(self.op == 'ifgoto'):
            print num,":", "if ", self.arg1, "goto ", self.result
        elif(self.op == 'param'):
            if(self.opType == 'STRING'):
                print num,": param STRING" +str(self.arg1)
            else:
                print num,":", "param " , self.arg1            
        elif(self.op == 'actparam'):
            print num,":", "actparam " , self.arg1
        elif(self.op == 'call'):
            if(self.arg1 == 'Out.Ln'):
                print num,":", "call " , self.arg1 , 0;
            else:
                print num,":", "call " , self.arg1, self.arg2
        elif(self.op == 'BEGIN_FUNC'):
            print num,":","BEGIN_FUNC ", self.arg1
        elif(self.op == 'END_FUNC'):            #while mapping to mips, this would be an unconditional return in case some branch of the function forgets to return
            print num,":","END_FUNC", self.arg1
        elif(self.op == 'return'):
            print num,":","return",self.arg1
        elif(self.op == 'BEGINMAIN'):
            print num,":", "BEGINMAIN";
        elif(self.op == 'ENDMAIN'):
            print num,":", "ENDMAIN" 
        elif(self.op == 'EXIT'):
            print num,":", "EXIT"  
        elif(self.op =='READINT' or self.op =='READREAL'):
            print num,":", self.op , self.arg1
        else:
            if(self.op is not None):
                if(self.op in unaryOps):
                    print num,":", self.result , " = " , self.op , self.arg1;
                elif(self.op in relOps):
                    print num,":", "if ", self.arg1 ,  self.op , self.arg2, " goto " , self.result;    
                elif(self.opType is None):
                    print num,":", self.result , " = " , self.arg1 , " " , self.op , " " , self.arg2;
                else:
                    print num,":", self.result , " = " , self.arg1 , " " , self.opType ," ",self.op , " " , self.arg2;         
            else:
                print num,":", self.result , " = " , self.arg1;             
#id is the lexeme of the variable
class PointerEntry(object):
    def __init__(self, id='', type='', offset=0, formalParameter = 0, callByReference = False, length = 0):
        self.id = id;
        self.type = type;
        self.offset = offset;
        self.formalParameter = formalParameter
        self.callByReference = callByReference
        self.length = length

class ArrayEntry(object):
    def __init__(self, id='', type='', offset=0, formalParameter = 0, callByReference = False, length = 0):
        self.id = id;
        self.type = type;
        self.offset = offset;
        self.formalParameter = formalParameter
        self.callByReference = callByReference
        self.length = length

class VarEntry(object):
    def __init__(self, id ='' , type='', offset=0, formalParameter = 0, callByReference = False, constant = False):
        self.id = id;
        self.type = type;
        self.offset = offset;
        self.formalParameter = formalParameter
        self.callByReference = callByReference
        self.constant = constant
        
#procName is the lexeme of the procedure
class ProcEntry(object):
    def __init__(self, procName='',type = [], returnType='' , pointer=None, forwardDeclaration = False):#pointer is pointer to symbol table of proc name
        self.procName = procName;
        self.type = type;
        self.returnType = returnType;
        self.pointer = pointer;
        self.forwardDeclaration = forwardDeclaration


class SymbolTable(object):
    def __init__(self, parent=None, offset = 0):
        self.parent = parent;
        self.symbolDictionary = {};
        self.offset = offset;
        
        
    def LookUp(self, id):
        if(id in self.symbolDictionary):
            return self.symbolDictionary[id];
        elif(self.parent == None):
            return None;
        else:
            return self.parent.LookUp(id);
    
    def Insert(self,entry):#while inserting into symbol table take care !!
        global flag2;
        if(entry.id in self.symbolDictionary):
            
            flag2 = False
        
            print "ERROR: Redefination of variable",entry.id;
            return 0;
        else:
            self.symbolDictionary[entry.id] = entry;
            return 1;
     
    
    def InsertProcedure(self,procEntry):
        global flag2;
        if(procEntry.procName in self.symbolDictionary):
            if not self.symbolDictionary[procEntry.procName].forwardDeclaration :

                flag2 = False
        
                print "ERROR: Redefination of variable",procEntry.procName;
                return 0;
            else:
                if(self.symbolDictionary[procEntry.procName].type == procEntry.type and self.symbolDictionary[procEntry.procName].returnType == procEntry.returnType):
                    self.symbolDictionary[procEntry.procName] = procEntry
                    if(debug2):
                        print "Current insertion is corresponding to some earlier forward declaration"; 
                    return 2;
                else:
                    print "ERROR : function definition does not match with declaration"

                    flag2 = False
                    return 3;
        else:     
            self.symbolDictionary[procEntry.procName] = procEntry;
            if(debug2):

                print "Successfuly entered function"
            return 1;
            
    def Print(self):
        global debug4
        if(debug4):
            print "\n \n PRINTING SYMBOL TABLE\n"
        r = ArrayEntry()
        
        r1 = ProcEntry()
        for id in self.symbolDictionary:    
            if(type(r)==type(self.symbolDictionary[id])):
                print id, self.symbolDictionary[id].type, self.symbolDictionary[id].offset, self.symbolDictionary[id].length
            elif(type(r1)==type(self.symbolDictionary[id])):
                print self.symbolDictionary[id].procName, "'s symbol table "
                self.symbolDictionary[id].pointer.Print()
                    
            else:
                print id, self.symbolDictionary[id].type, self.symbolDictionary[id].offset
            
        print "\n\n"
###########################################################################################
#Lexer
###########################################################################################
import sys
sys.path.insert(0,"../..")

if sys.version_info[0] >= 3:
    raw_input = input
    
import ply.lex as lex

literals = ['+','-','*','/','=','<','>',';',',','.',':','~','&','(',')','#','{','}','[',']','^','|']

#builtIns = []

reserved = {
        # BUILT-INS -- SEPARATE DEPENDING ON NUMBER OF PARAMETERS
        'ABS' : 'ABS',
        'ASH' : 'ASH',
        'CAP' : 'CAP',
        'CHR' : 'CHR',
        'MAX' : 'MAX',
        'MIN' : 'MIN',
        'ORD' : 'ORD',
        'SIZE' : 'SIZE',
        'DEC' : 'DEC',
        'HALT' : 'HALT',
        'INC' : 'INC',
        'READINT' : 'READINT',
        'READREAL' : 'READREAL',

# Activate at the time of semantic analysis
        
            # KEYWORDS
            
            #The following Keywords are responsible for control flow
            'WHILE' : 'WHILE',
            'ELSE' : 'ELSE',
            'IF' : 'IF',
            'THEN' : 'THEN',
            'ELSIF' : 'ELSIF',
            'BEGIN' : 'BEGIN',
            'END' : 'END',
            'RETURN' : 'RETURN',
            'UNTIL' : 'UNTIL',            
            'LOOP' : 'LOOP',
            'DO' : 'DO',
            'FOR' : 'FOR',
            
        #The following keywords are to be implemented in case of extensions
            'RECORD' : 'RECORD',
            'POINTER' : 'POINTER',
            'CASE' : 'CASE',
            'REPEAT' : 'REPEAT',
            'WITH' : 'WITH',
            'EXIT' : 'EXIT',
            'BY' : 'BY',
                                                
            #The Syntax for the procedure is given by:
            #PROCEDURE <name >
            #BEGIN
            #...
            #END <name >
            #The following keywordxs are needed for the same.
            'PROCEDURE' : 'PROCEDURE',
            'DEFINITION' : 'DEFINITION',
            'IMPORT' : 'IMPORT',
            'TYPE' : 'TYPE',
            'MODULE' : 'MODULE',
            'VAR' : 'VAR',

            # Operators (Logical and arithmetic)
            'IN' : 'IN',
            'TO' : 'TO',
            'MOD' : 'MOD',
            'OR' : 'OR',
            'DIV' : 'DIV',
            'OF' : 'OF',
        
            # The following Keywords shall be activated in case of Extensions
            # 'TRUE': 'TRUE',
            # 'FALSE' : 'FALSE',

            # The following keywords shall be needed for the basic and complex types shall be implemented
            'INTEGER' : 'INTEGER',
            'REAL' : 'REAL',
            'BOOLEAN' : 'BOOLEAN',
            'CHAR' : 'CHAR',
            'CONST' : 'CONST',
            'ARRAY' : 'ARRAY',
            
            'NIL' : 'NIL'
}

tokens = [

    #The following are the Contstants that have been implemented.
    'ID',
    'INTEGERCONSTANT',
    'REALCONSTANT',
    'CHARACTERCONSTANT',
    'STRINGCONSTANT',
    'BOOLEANCONSTANT',
    
    #To be implemented
    #'STRING-LITERAL'

    #The following are the operators that have been implemented
    'ASSIGN',
    'LESSEQUAL',
    'MOREEQUAL',
    'COMMENT',
    'DOTDOT'
]
    
tokens += reserved.values()


def t_BOOLEANCONSTANT(t): # Boolean Constants
    r'TRUE|FALSE'
    return t


def t_ID(t):    # Check for reserved words
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_REALCONSTANT(t):     

    #r'\d*\.\d+'
    r'(\d*\.\d+([eE][+-]?\d+)?|\d+[eE][+-]?\d+)' # for Real Constants including those of the form 3.14 and 6.36e-34
    
    #print t.value;
    t.value= float(t.value);
    print t.value, type(t.value)
    #t.value = int(t.value)
    return t
    
def t_INTEGERCONSTANT(t):
    r'\d+'
    t.value = int(t.value)  # for Real Constants including those of the form 42
    return t

def t_COMMENT(t):
    r'\(\*(.|\n)*?\*\)' # for Comments of the form (* ...Comments... *) 
    #print t;
    pass; # Do Nothing
    #return t

def t_CHARACTERCONSTANT(t):
#   r"\'\\t\'" 
    r'(\'(.|\\n|\\t)\')'#|\"(.|\\n|\\t)\")' #value assignment to escape sequences for \n and \t
    #print len(t.value);
    return t
 
def t_STRINGCONSTANT(t): 
    r'(\"(.|\\n|\\t)*\"|\'(.|\\n|\\t)*\')' # for String constants: Double Quotes with something in it. Special Treatment to \n and \t
    return t  
    
def t_newline(t):
    r'\n+' # for the newline character
    #t.lexer.lineno += t.value.count("\n")
    t.lexer.lineno += len(t.value)
    #return t
    #print t.lexer.lineno
    
# for the recognition of the assignment, Less-than-equal-to, Greater-than-equal-to, dot-dot and the ignore operators
t_ASSIGN = r':='
t_LESSEQUAL = r'<='
t_MOREEQUAL = r'>='
t_DOTDOT = r'\.\.'
t_ignore = ' \t'

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)



# Here, the lexer is built.
import ply.lex as lex
lexer = lex.lex();#debug=True);

###########################################################################################
#Parser
###########################################################################################


#TODO
#Global variables
variables = [];
moduleDictionary = {}; # Names of modules imported
moduleList = {}
moduleExpList = {}
moduleList['In'] = ['Int', 'Real']

moduleReturnTypes = {}
moduleReturnTypes['Out'] = {}
moduleReturnTypes['Out']['Int'] = 'VOID'
moduleReturnTypes['Out']['Real'] = 'VOID'
moduleReturnTypes['Out']['String'] = 'VOID'
moduleReturnTypes['Out']['Char'] = 'VOID'
moduleReturnTypes['Out']['Ln'] = 'VOID' # might create problems

moduleReturnTypes['In'] = {}
moduleReturnTypes['In']['Int'] = 'INTEGER'
moduleReturnTypes['In']['Real'] = 'REAL'



moduleExpList['Out'] = {}
moduleExpList['Out']['Int']  = ['INTEGER'];#, 'INTEGER']
moduleExpList['Out']['Real']  = ['REAL'];#, 'INTEGER', 'INTEGER']
moduleExpList['Out']['String']  = ['STRINGCONSTANT'];
moduleExpList['Out']['Char']  = ['CHAR'];

moduleList['Out'] = ['Int', 'Real', 'String', 'Ln', 'Char']  #can remove ln

# current function
# Call by value or reference ! -- Just check for 'VAR' 
passByReference = False;
# nesting Depth
# Insert markers after procedure name and after 'END' of it!
# Add some erraneous constructs in the grammar

symbolTable = SymbolTable()
stack = [symbolTable]

formalParameterNo = 0
start = 'module'

#Broadly, the following specification are responsible for the declarations of Modules, and the lists to be imported.

def p_module(t):
    '''module : MODULE ID ';' importList declarationList statementBlock END ID '.' ''';
    if(t[2]!=t[8]):
        global flag2
        flag2 = False
        print "ERROR : module names do not match in line no ", t.lineno(8)
    #print "lineno ,  ", t.lexer.lineno
    
#Erraneous construct -- commonly done by programmers!
def p_module2(t):
    '''module : MODULE ID ';' importList declarationList statementBlock END ID ''';
    global flag2;
    if(t[2]!=t[8]):

        flag2 = False
        
        print "ERROR : module names do not match in line no ", t.lineno(8)

    flag2 = False
        
    print "ERROR : Full stop missing in the end in line no", t.lineno(8)    

def p_importList(t):
    '''importList   : IMPORT importedModulesList ';'
                    |     ''';
    if(debug2):
        print("Imported Modules");                
        print moduleDictionary;
        print moduleDictionary.keys();#These cannot be declared as variables
                    
def p_importedModulesList(t):
    '''importedModulesList : importedModules ',' importedModulesList
                        | importedModules''' ;
    
def p_importedModules(t):
    '''importedModules    :      ID ASSIGN ID 
    | ID '''
    global flag2;
    if(len(t)==2):
        if(t[1]!="In" and t[1]!="Out"):                 #check if imported modules are either of In and Out
            
            flag2 = False
        
            print "ERROR: in importing module ", t[1] , " at line no ", t.lineno(1)
        else:
            t[0] = t[1]
            moduleDictionary[t[1]]=t[1];
    if(len(t)==4):
        if(t[3]!="In" and t[3]!="Out"):
           
            flag2 = False
        
            print "ERROR: in importing module ", t[3] , " at line no ", t.lineno(2)
        else:
            t[0] = t[3]
            moduleDictionary[t[1]]=t[3];
    
        #create a symbol table entry for module names 

#Broadly, the following specification are responsible for the declarations of Identifiers, constants, variables, procedures, formal parameters and of declarations

def p_declarationList(t):
    '''declarationList      :       identifierDeclarationList procedureDeclarationList    '''
    
def p_identifierDeclarationList(t):
    '''identifierDeclarationList      :      CONST constantDeclarationList identifierDeclarationList 
    | VAR   variableDeclarationList   identifierDeclarationList
    |    ''';
    
  
    
def p_constantDeclarationList(t):
    '''constantDeclarationList :      ID '=' expression ';' constantDeclarationList
    |    ''';
    global stack
    global intSize, realSize ,charSize, booleanSize
    global constList
    symbolTable = stack[len(stack)-1]
    if(len(t) > 1):
        currentOffset = symbolTable.offset

        if(t[3]['type']=='INTEGER'):
            symbolTable.offset = symbolTable.offset + intSize
        elif(t[3]['type']=='REAL'):
            symbolTable.offset = symbolTable.offset + realSize
        elif(t[3]['type']=='CHAR'):
            symbolTable.offset = symbolTable.offset + charSize
        elif(t[3]['type']=='BOOLEAN'):
            symbolTable.offset = symbolTable.offset + booleanSize
    
        entry = VarEntry(t[1], t[3]['type'], currentOffset, 0, False, True);

        symbolTable.Insert(entry);
        
        quad = Quad(None, t[3]['place'], None, t[1], t[3]['type'])
        constList.append(quad)
        
def p_variableDeclarationList(t):
    '''variableDeclarationList  :       identifierList ':' type ';' clearVariables variableDeclarationList
    |''';
    #global variables
    #if(len(t) > 1):
    #   for i in variables:
    #      print (i,t[3]);
            #variables.remove(i)
            #print variables
        
def p_clearVariables(t):
    ''' clearVariables : '''
    global stack
    global variables
    global debug2
    global flag2
    global debug3
    global intSize
    global realSize
    global charSize
    global booleanSize
    if(debug2):
        print("Printing variables");
    symbolTable = stack[len(stack)-1]
    for i in variables:
        if(t[-2]['type']=='BOOLEAN' or t[-2]['type']=='INTEGER' or t[-2]['type']=='REAL' or t[-2]['type']=='CHAR'):
            if(debug2):
                print "Adding variable entry"
            currentOffset = symbolTable.offset

            if(t[-2]['type']=='INTEGER'):
                symbolTable.offset = symbolTable.offset + intSize
            elif(t[-2]['type']=='REAL'):
                symbolTable.offset = symbolTable.offset + realSize
            elif(t[-2]['type']=='CHAR'):
                symbolTable.offset = symbolTable.offset + charSize
            elif(t[-2]['type']=='BOOLEAN'):
                symbolTable.offset = symbolTable.offset + booleanSize
            
            entry = VarEntry(i, t[-2]['type'], currentOffset)
            symbolTable.Insert(entry)
            if(debug2):
                print (i,t[-2]['type']);
        
        elif(t[-2] is not None): 
            if(len(t[-2])>0 ):
                if(t[-2]['type']=='ARRAY'):
                    if(debug2):
                        print "Adding array entry"
                    if(t[-2]['arrayLength'] is None ):
                       
                        flag2 = False
        
                        print "ERROR : Array size not known at compile time" , " at line no ", t.lineno(-1)
                    elif(t[-2]['arrayLength'] <0 ):                        
                        print "ERROR : Negative array size" , " at line no ", t.lineno(-1)
                        
                        flag2 = False        
                    else:
                        currentOffset = symbolTable.offset
                        arrayLength = t[-2]['arrayLength'];

                        if(t[-2]['arrayOf']['type']=='INTEGER'):
                            symbolTable.offset = symbolTable.offset + intSize*arrayLength
                        elif(t[-2]['arrayOf']['type']=='REAL'):
                            symbolTable.offset = symbolTable.offset + realSize*arrayLength
                        elif(t[-2]['arrayOf']['type']=='CHAR'):
                            symbolTable.offset = symbolTable.offset + charSize*arrayLength
                        elif(t[-2]['arrayOf']['type']=='BOOLEAN'):
                            symbolTable.offset = symbolTable.offset + booleanSize*arrayLength

                        entry = ArrayEntry(i, t[-2]['arrayOf'], currentOffset, False, False, t[-2]['arrayLength'])

                        symbolTable.Insert(entry)
                        if(debug2):
                            print (i,t[-2]);
                elif(t[-2]['type']=='POINTER'):
                    if(debug2):
                        print "Adding pointer entry"
                    if(t[-2]['arrayLength'] is None ):
                        print  "ERROR : Array size not known at compile time" , " at line no ", t.lineno(-1)
                        
                        flag2 = False
        
                    elif(t[-2]['arrayLength'] <0 ):
                        print  "ERROR : Negative array size" , " at line no ", t.lineno(-1)
                        
                        flag2 = False
        
                    else:                        
                        currentOffset = symbolTable.offset
                        symbolTable.offset += pointerSize
                        arrayLength = t[-2]['arrayLength'];
                        #print arrayLength , "arrayLength";
                        #print t[-2]['arrayOf']
                        if(t[-2]['arrayOf']['type']=='INTEGER'):
                            #print "old offset ", symbolTable.offset
                            symbolTable.offset = symbolTable.offset + intSize*arrayLength
                            #print "new offset ", symbolTable.offset
                        elif(t[-2]['arrayOf']['type']=='REAL'):
                            symbolTable.offset = symbolTable.offset + realSize*arrayLength
                        elif(t[-2]['arrayOf']['type']=='CHAR'):
                            symbolTable.offset = symbolTable.offset + charSize*arrayLength
                        elif(t[-2]['arrayOf']['type']=='BOOLEAN'):
                            symbolTable.offset = symbolTable.offset + booleanSize*arrayLength
                        
                        
                        
                        entry = PointerEntry(i, t[-2]['arrayOf'], currentOffset, False, False, t[-2]['arrayLength'])
                        symbolTable.Insert(entry)
                        if(debug2):
                            print (i,t[-2]);
    variables=[];   
    if(debug2):
        symbolTable.Print()
    #print variables
    # Also print the nesting depth and function in which it is declared

def p_identifierList(t):
    '''identifierList    :       ID 
    | ID ',' identifierList    ''';
    global variables;
    global moduleDictionary;
    t[0] = {}
    
    if(t[1] in moduleDictionary):
        print "ERROR : Variable ",t[1] ," already in use as a module name" , " at line no ", t.lineno(1)
        global flag2
        flag2 = False
        
    else:
        variables.append(t[1]);
    if(len(t)==2):
        t[0]['place'] = [t[1]]
    else:
        t[0]['place'] = t[3]['place']
        t[0]['place'].append(t[1])
    
    #print variables
    
def p_procedureDeclarationList(t):
    '''procedureDeclarationList     :      procedureDeclaration ';' procedureDeclarationList
    | forwardDeclaration ';' procedureDeclarationList
    |'''    ;

def p_procedureDeclaration(t):
    '''procedureDeclaration     :       PROCEDURE receiver ID createTable optionalFormalParameters ';' declarationList insertEntry statementBlock END ID popTable''';
    
    #quad1 = Quad('BEGIN_FUNC',t[3]);#to be done in createTable
    #emit(quad1);
    #quad2 = Quad()
    
    
def p_insertEntry(t):
    '''insertEntry : '''
    global stack
    if(debug2):
        print "inserting symbol table"
    oldSymbolTable = stack[len(stack)-2] 
    newSymbolTable = stack[len(stack)-1] 
    
    #procName =  t[-1];
    procName =  t[-5];
    
    #returnType =  t[-6]['returnType']
    returnType =  t[-3]['returnType']
    
    var = VarEntry()
    inputType = []
    inputType2 = []
    
    
    count = len(newSymbolTable.symbolDictionary)
    
    for i in range(0, count+1):
        inputType.append('')
        
    for i in newSymbolTable.symbolDictionary:
        if(type(newSymbolTable.symbolDictionary[i]) == type(var)):
            
            #if( type(newSymbolTable.symbolDictionary[i].formalParameter)):
            #print "sumit    ", ( type(newSymbolTable.symbolDictionary[i].formalParameter))
            if( newSymbolTable.symbolDictionary[i].formalParameter != 0):
                
                inputType[newSymbolTable.symbolDictionary[i].formalParameter] = newSymbolTable.symbolDictionary[i].type
                #print "noooooooooooooo" , newSymbolTable.symbolDictionary[i].formalParameter
    
    for i in inputType:
        if(i!=''):
            inputType2.append(i)
    
    
    '''for i in newSymbolTable.symbolDictionary:
        if(type(newSymbolTable.symbolDictionary[i]) == type(var)):
            
            if( type(newSymbolTable.symbolDictionary[i].formalParameter)):
                inputType.append(newSymbolTable.symbolDictionary[i].type)
    '''
    if(debug2):
        print "printing input type !!!!!!!!!!\n" , inputType2
        print "printing return type !!!!!!!\n", returnType
        print "printing procedure name !!!!!!!\n", procName
        
    entry = ProcEntry(procName, inputType2, returnType, newSymbolTable)
    oldSymbolTable.InsertProcedure(entry)
    if(debug2):
        print len(stack)
        
    quad1 = Quad('BEGIN_FUNC',procName);
    emit(quad1);
    
def p_popTable(t):
    '''popTable : '''
    global stack
    if(debug2):
        print "popping symbol table"
    oldSymbolTable = stack[len(stack)-2] 
    newSymbolTable = stack[len(stack)-1] 
    
    procName =  t[-1];
    #procName =  t[-5];
    
    if(debug2):
        print len(stack)
        stack.pop().Print()
    else:
        stack.pop()
    
    quad = Quad('END_FUNC', procName);
    emit(quad)
    
def p_createTable(t):
    '''createTable : '''
    global stack
    if(debug2):
        print "\nCreating table \n\n"
    oldSymbolTable = stack[len(stack)-1] 
    newSymbolTable = SymbolTable(oldSymbolTable)
    if(debug2):
        print t[-1];
    #entry = ProcEntry(t[-1],'','',newSymbolTable)
    #oldSymbolTable.InsertProcedure(entry)
    stack.append(newSymbolTable)
    
    #quad1 = Quad('BEGIN_FUNC',t[-1]);
    #emit(quad1);
    #quad2 = Quad()
    if(debug2):
        print "Old symbol table"
        oldSymbolTable.Print()
        print "New symbol table"
        newSymbolTable.Print()
        
def p_receiver(t):
    '''receiver     :       '(' optionalVAR ID  ':' ID  ')'
    |    ''';
    if(len(t)>1):
        if(debug2):
            print("receiver found");

def p_optionalVAR(t):
    '''optionalVAR       :      VAR
    |    ''';
    global passByReference;
    if(len(t)>1):
        if(debug2):
            print("Passing by reference");
        passByReference = True;

def p_optionalFormalParameters(t):
    '''optionalFormalParameters :       '(' optionalFormalParameterList ')' ':' qualident
    | '(' optionalFormalParameterList ')' 
    |''';
    t[0] = {}
    if(len(t)==6):
        t[0]['returnType'] = t[5]
    else:
        t[0]['returnType'] = "VOID"
    global formalParameterNo
    formalParameterNo = 0
    
    if(len(t)>1):
        t[0]['place'] = t[2]['place']
    else:
        t[0]['place'] = []
    t[0]['place'].reverse()
    for i in t[0]['place']:
        quad = Quad('actparam', i)
        #var = VarEntry(i, )
        emit(quad)
def p_optionalFormalParameterList(t):
    '''optionalFormalParameterList    :      formalParameterList
    |    ''';
    t[0] = {}
    if(len(t)==1):
        t[0]['place'] = []
    else:
        t[0]['place'] = t[1]['place']
    
def p_formalParameterList(t):
    '''formalParameterList       :      formalParameter ';' formalParameterList
    | formalParameter   ''';
    t[0] = {}
    
    if(len(t)==2):
        t[0]['place'] = t[1]['place']
    else:
        t[0]['place'] = t[1]['place']
        t[0]['place'].extend(t[3]['place'])
    
def p_formalParameter(t):
    '''formalParameter  :      optionalVAR identifierList ':' type '''
    # Doubtful in t[4] -- take care.
    global debug2
    global intSize
    global realSize
    global charSize
    global booleanSize
    t[0] = {}
    if(debug2):
        print("Printing Formal Parameters");
    global variables;
    global passByReference;
    global stack
    symbolTable = stack[len(stack)-1]
    global formalParameterNo
    for i in variables:
        formalParameterNo += 1
        currentOffset = symbolTable.offset

        if(t[4]['type']=='INTEGER'):
            symbolTable.offset = symbolTable.offset + intSize
        elif(t[4]['type']=='REAL'):
            symbolTable.offset = symbolTable.offset + realSize
        elif(t[4]['type']=='CHAR'):
            symbolTable.offset = symbolTable.offset + charSize
        elif(t[4]['type']=='BOOLEAN'):
            symbolTable.offset = symbolTable.offset + booleanSize

        entry = VarEntry(i,t[4]['type'], currentOffset, formalParameterNo, passByReference)

        #entry = VarEntry(i,t[4]['type'], 0, formalParameterNo, passByReference)
        symbolTable.Insert(entry)
        if(debug2):
            print (i,t[4]['type']) , passByReference;   
    variables=[];
    passByReference = False;
    #print(t[4]);
    
    t[0]['place'] = t[2]['place']
    
def p_forwardDeclaration(t):
    '''forwardDeclaration  :       PROCEDURE '^' receiver ID createTable optionalFormalParameters popTable2''';

def p_popTable2(t):
    '''popTable2 : '''
    global stack
    if(debug2):
        print "popping symbol table"
    oldSymbolTable = stack[len(stack)-2] 
    newSymbolTable = stack[len(stack)-1] 
    procName =  t[-3];
    returnType =  t[-1]
    
    var = VarEntry()
    inputType = []
    inputType2 = []
    
    count = len(newSymbolTable.symbolDictionary)
    
    for i in range(0, count+1):
        inputType.append('')
        
    for i in newSymbolTable.symbolDictionary:
        if(type(newSymbolTable.symbolDictionary[i]) == type(var)):
            
            #if( type(newSymbolTable.symbolDictionary[i].formalParameter)):
            if(debug2):
                print "type of formal parameter 1    ", ( type(newSymbolTable.symbolDictionary[i].formalParameter))
            if( newSymbolTable.symbolDictionary[i].formalParameter != 0):
                
                inputType[newSymbolTable.symbolDictionary[i].formalParameter] = newSymbolTable.symbolDictionary[i].type
                if(debug2):
                    print "type of formal parameter 2    " , newSymbolTable.symbolDictionary[i].formalParameter
    
    for i in inputType:
        if(i!=''):
            inputType2.append(i)
    
    if(debug2):    
        print "printing input type !!!!!!!!!!\n" , inputType2
        print "printing return type !!!!!!!\n", returnType
        print "printing procedure name !!!!!!!\n", procName
        
    entry = ProcEntry(procName, inputType2, returnType, newSymbolTable, True)
    oldSymbolTable.InsertProcedure(entry)
    if(debug2):
        stack.pop().Print()
    else:
        stack.pop()
#def p_popTable2(t):
#    '''popTable2 : '''
#    global stack
#    print "popping symbol table"
#    oldSymbolTable = stack[len(stack)-2] 
#    newSymbolTable = stack[len(stack)-1] 
#    procName =  t[-3];
#    returnType =  t[-1]
#    entry = ProcEntry(procName, '', returnType, newSymbolTable, True)
#    oldSymbolTable.InsertProcedure(entry)
#    stack.pop().Print()

# Broadly, The following specification are responsible for the parsing of the statements and lists of statements

def p_statementBlock(t):
    '''statementBlock  : BEGIN beginquad statementList boolMarker
                    |     '''
    if(len(t)==5):
        backPatch(t[3]['nextList'], t[4]['quad'])
        #pass

    global stack;
    if(len(stack)==1):
        quad = Quad('ENDMAIN');
        emit(quad);
        
def p_beginquad(t):
    '''beginquad :'''
    global stack;
    global constList
    if(len(stack)==1):
        quad = Quad('BEGINMAIN');
        emit(quad);
        for i in constList:
            emit(i)

def p_statementList(t):
    '''statementList :       statementList boolMarker statement ';' 
    | statement ';'
    |    ''';
    global debug3
    global debug4
    t[0] = {}
    if(len(t)==3):
        
        t[0]['nextList'] = t[1]['nextList']
    elif(len(t)==5):
        backPatch(t[1]['nextList'], t[2]['quad'])
        if(debug3):
            print "Printing backpatching", t[1]['nextList'], t[2]['quad']
        t[0]['nextList'] = t[3]['nextList']
    else:
        t[0]['nextList'] = []
        if(debug4):
            print "Empty statement list"
        
    
def p_statement_if(t):
    '''statement : IF expression THEN boolMarker statementList nMarker boolMarker elsifList optionalElse END'''
    t[0] = {}
    if(t[2]['type']!='BOOLEAN'):
        print "ERROR : Expression must be of type BOOLEAN" , " at line no ", t.lineno(1)
        global flag2
        flag2 = False
    else:
        backPatch(t[2]['trueList'], t[4]['quad'])
        backPatch(t[2]['falseList'], t[7]['quad'])
        t[0]['nextList'] = merge(t[5]['nextList'], t[6]['nextList'])
        auxList = merge(t[8]['nextList'], t[9]['nextList'])
        t[0]['nextList'] = merge(t[0]['nextList'], auxList)
    #S is t[2]
    #S1 is t[4]

def p_elsifList(t):
    '''elsifList    :      ELSIF expression THEN boolMarker statementList nMarker boolMarker elsifList
    |    ''';
    t[0] = {}
    if(len(t)>1):
        if(t[2]['type']!='BOOLEAN'):
            print "ERROR : Expression must be of type BOOLEAN" , " at line no ", t.lineno(1)
            global flag2
            flag2 = False
        else:
            backPatch(t[2]['trueList'], t[4]['quad'])
            backPatch(t[2]['falseList'], t[7]['quad'])
            t[0]['nextList'] = merge(t[5]['nextList'], t[6]['nextList'])
            t[0]['nextList'] = merge(t[0]['nextList'], t[8]['nextList'])

    else:
        
        t[0]['nextList'] = []
        #t[0]['code'] = []
        
def p_optionalElse(t):
    '''optionalElse      :      ELSE statementList
    |    ''';
    t[0] = {}
    global debug4
    if(debug4):
        print "t0", t[0]
    if(len(t)==3):
        t[0]['code'] = []
        t[0]['nextList'] = t[2]['nextList']
    else:        
        t[0]['code'] = []
        t[0]['nextList'] = []


def p_statement_case(t):
    '''statement    : CASE expression OF caseList optionalElse END'''
    t[0]={}
    t[0]['type'] = "VOID"
    t[0]['nextList'] = []
    global flag2
    if(t[2]['type']=='CHAR' or t[2]['type']=='INTEGER'):
        pass;
    else:
        flag2 = False;
        print "ERROR : Expression should be of type integer or character, at line no", t.lineno(1);
    
    stillTestingCase = True
    for i in t[4]['type']:
        for j in t[4]['type']:
            if((i != j) and stillTestingCase):
                print "ERROR : Incompatible types in caseList ", i ,j , " at line no ", t.lineno(1)
                flag2 = False
                stillTestingCase = False
    
    stillTestingCase = True        
    for ind in t[4]['type']:
        if((ind != t[2]['type']) and stillTestingCase):
            print "ERROR: Incompatible types in case statement", ind , t[2]['type'] , " at line no ", t.lineno(1)
            flag2 = False
            stillTestingCase = False
    
    
            
def p_caseList_recur(t):
    '''caseList     : case '|' caseList    ''';    
    t[0]={}
    global debug2
    if(debug2):
        print "caselist : recursive -- case: ", t[1];
        print "caselist : recursive -- caseList: ", t[3];
    t[3]['type'].extend(t[1]['type']);
    t[0]['type'] = t[3]['type']
    if(debug2):
        print "caselist : recursive -- new caseList : ", t[0];
        
        
def p_caseList(t):
    '''caseList     :      case    ''';
    t[0] = {}
    t[0]['type'] = t[1]['type'];
    global debug2
    if(debug2):
        print "caselist to case : non-recursive : ", t[0];

def p_case(t):
    '''case         :       expression ':' statementList    ''';
    t[0]={}
    global debug2;
    if(debug2):
        print "case label list (types): ",t[1]
    t[0]['type'] = [t[1]['type']]; # Passing the types
        
def p_case_empty(t):
    '''case         :    ''';    
    t[0] = {};
    t[0]['type'] = [];    

"""    
def p_case(t):
    '''case         :       caseLabelList ':' statementList    ''';
    t[0]={}
    global debug2;
    if(debug2):
        print "case label list (types): ",t[1]
    t[0]['type'] = t[1]['type']; # Passing the types
        
def p_case_empty(t):
    '''case         :    ''';    
    t[0] = {};
    t[0]['type'] = [];
    
def p_caseLabelList_recursive(t):
    '''caseLabelList :      caseLabels ',' caseLabelList   ''';
    t[0]= {};
    global debug2;
    if(debug2):
            print "CASE label list : recursive call 1 ", t[3]
            print "CASE label list : recursive call 2 ", t[1]
    t[3]['type'].append(t[1]['type'])
    t[0]['type'] = t[3]['type']
    if(debug2):
        print "CASE label list : recursive call 3", t[0]
                
def p_caseLabelList(t):
    '''caseLabelList :      caseLabels    ''';
    t[0] = {};
    global debug2;
    t[0]['type'] = [t[1]['type']];
    if(debug2):
        print "CASE label list : non-recursive call", t[0]
            
def p_caseLables(t):
    '''caseLabels   :       expression     ''';
    t[0] = {}
    t[0]['type'] = t[1]['type'];

def p_caseLables_two(t):
    '''caseLabels   : expression DOTDOT expression   '''
    t[0] = {}
    # My fix -- Sumit
    # Check it.
    if(t[1]['type'] != t[2]['type']):
        print "ERROR : Error in types" , " at line no ", t.lineno(2)
        global flag2
        flag2 = False
    t[0]['type'] = t[1]['type'];"""
    

def p_statement_while(t):
    '''statement    : WHILE boolMarker expression DO boolMarker statementList END'''
    global debug3   
    t[0] = {}
    if(t[3]['type']!='BOOLEAN'):
        print "ERROR : Expression must be of type BOOLEAN" , " at line no ", t.lineno(1)
        global flag2
        flag2 = False
    else:
        if(debug3):
            print "Printing while backpatching", t[6]['nextList'], t[2]['quad']
        backPatch(t[6]['nextList'], t[2]['quad'])
        if(debug3):
            print "Printing while backpatching", t[3]['trueList'], t[5]['quad']
        backPatch(t[3]['trueList'], t[5]['quad'])
        t[0]['nextList'] = t[3]['falseList']
        quad = Quad('goto', None, None, t[2]['quad'])
        emit(quad)
        
    
def p_statement_repeat(t):
    '''statement    : REPEAT boolMarker statementList UNTIL boolMarker expression'''
    t[0] = {}
    if(t[6]['type']!='BOOLEAN'):
        print "ERROR : Expression must be of type BOOLEAN" , " at line no ", t.lineno(1)
        global flag2
        flag2 = False
    else:
        backPatch(t[6]['falseList'], t[2]['quad'])
        backPatch(t[3]['nextList'], t[5]['quad'])
        t[0]['nextList'] = t[6]['trueList']
    
def p_statement_for(t):
    '''statement    : FOR ID  ASSIGN expression forAssign TO expression toValue optionalBY DO boolMarker statementList boolMarker END'''   
    var = VarEntry()
    global stack;
    global targetList;
    global flag2;
    symbolTable = stack[len(stack)-1]
    relEntry = symbolTable.LookUp(t[2])
    t[0] = {}
    t[0]['nextList'] = []
    t2f = False
    if(relEntry is None):
        print "ERROR : Variable assignment before declaration" , " at line no ", t.lineno(3)
        flag2 = False
    else:
        if(type(relEntry) != type(var)):
            print "ERROR : Only variables allowed at line no ", t.lineno(3)
            flag2 = False    
        else:#if ID is a valid identifier
            if(relEntry.type=='INTEGER'):
                if(t[4]['type']!='INTEGER' or t[7]['type']!='INTEGER'):
                    print "ERROR : Expression must be of type INTEGER" , " at line no ", t.lineno(3)
                    flag2 = False
            else:
                print "ERROR : Identifier must be of type INTEGER" , " at line no ", t.lineno(3)
                flag2 = False
                
            if(flag2 is not False):#Checking if step value is 0
                if(t[9]['value'] is not None):
                    if(t[9]['value']==0):
                        print "ERROR: Zero step value is not allowed"
                        flag2 = False
            if(flag2 is not False):
                if(t[4]['value'] is not None and t[7]['value'] is not None):
                    if(t[4]['value']>t[7]['value']):
                        t2f = True
                    if(t2f and t[9]['value'] is not None):
                        if(t[9]['value']>=0):
                            print "ERROR: Negative step value expected at line no ", t.lineno(3)
                            flag2 = False
                    if(not t2f and t[9]['value'] is not None):
                        if(t[9]['value']<=0):
                            print "ERROR: Positive step value expected at line no ", t.lineno(3)
                            flag2 = False
            if(flag2 is not False):
                quad1 = Quad('+', relEntry.id, t[9]['place'], relEntry.id, 'INTEGER')
                emit(quad1)
                quad2 = Quad('<=', t[5]['from'], t[8]['to'], nextQuad()+3, 'INTEGER')
                targetList.append(nextQuad()+3);
                quad3 = Quad('>=', relEntry.id, t[7]['place'], t[11]['quad'], 'INTEGER')
                targetList.append(t[11]['quad']);
                quad4 = Quad('goto', None, None, nextQuad()+4);
                targetList.append(nextQuad()+4);
                quad5 = Quad('<=', relEntry.id, t[7]['place'], t[11]['quad'], 'INTEGER')
                targetList.append(t[11]['quad']);
                emit(quad2)
                emit(quad3)
                emit(quad4)
                emit(quad5)
                backPatch(t[12]['nextList'], t[13]['quad'])

def p_forAssign(t):
    '''forAssign    : ''';
    t[0] = {}
    quad = Quad(None, t[-1]['place'], None, t[-3])
    emit(quad)
    temp = newTemp(t[-1]['type'])
    quad = Quad(None, t[-1]['place'], None, temp)
    emit(quad)
    t[0]['from'] = temp

def p_toValue(t):
    '''toValue    : ''';
    t[0] = {}
    temp = newTemp(t[-1]['type'])
    quad = Quad(None, t[-1]['place'], None, temp)
    emit(quad)
    t[0]['to'] = temp
    
def p_optionalBY(t):
    '''optionalBY        :      BY expression
    |    ''';
    global flag2;
    t[0] = {}
    if(len(t)>1):
        if(t[2]['type']!='INTEGER'):
            print "ERROR : Expression must be of type INTEGER"
            flag2 = False
        else:
            if(t[2] is None):
                print "ERROR: Step value missing at line no ", t.lineno(1)
                flag2 = False
            else:
                t[0]['value'] = t[2]['value']
                t[0]['place'] = t[2]['place']
    else:
        t[0]['value'] = 1
        t[0]['place'] = t[0]['value']
        
def p_statement_loop(t):
    '''statement    : LOOP boolMarker statementList END'''
    global targetList;
    t[0] = {}
    t[0]['nextList'] = []
    quad = Quad('goto', None, None, t[2]['quad']);
    targetList.append(t[2]['quad']);
    emit(quad)
    
def p_statement_exit(t):
    '''statement    :  EXIT''' 
    t[0] = {}
    t[0]['nextList'] = []
    quad = Quad('EXIT');
    emit(quad);

def p_statement_read_int(t):
    '''statement    :  READINT '(' designator ')' ''' 
    global stack;
    global flag2;
    global moduleDictionary;
    t[0] = {};
    t[0]['nextList'] = [];    
    symbolTable = stack[len(stack)-1];
    if('In' not in moduleDictionary.values()):
        print "ERROR : 'In' module needs to be imported"
        flag2 = False;
    else:                
        temp = t[3]['type']; # Handling direct integer variables -- no arrays or pointers
        #dict = {};
        #if(type(temp) == type(dict)):
        #    temp = temp['type']
        if(temp != 'INTEGER'):
            print "ERROR : READINT Unsuccessful. Type Mismatch";
            flag2 = False;
        else:    

            #print "################", temp, t[3]['place']
        
            quad = Quad('READINT', t[3]['place']);
            emit(quad);    
        
def p_statement_read_real(t):
    '''statement    :  READREAL '(' designator ')' ''' 
    global stack;
    global flag2;
    global moduleDictionary;
    t[0] = {};
    t[0]['nextList'] = [];
    symbolTable = stack[len(stack)-1];
    if('In' not in moduleDictionary.values()):
        print "ERROR : 'In' module needs to be imported"
        flag2 = False;
    else:                
        temp = t[3]['type']; # Handling direct integer variables -- no arrays or pointers
        #dict = {};
        #if(type(temp) == type(dict)):
        #    temp = temp['type']
        if(temp != 'REAL'):
            print "ERROR : READREAL Unsuccessful. Type Mismatch";
            flag2 = False;
        else:    

            #print "################", temp, t[3]['place']
        
            quad = Quad('READREAL', t[3]['place']);
            emit(quad);    
                
    
def p_statement_return(t):
    '''statement    :   RETURN '''
    t[0] = {}
    t[0]['nextList'] = [];## galat hai -- change karna hai
    quad = Quad('return');
    emit(quad);    
    
def p_statement_return_expression(t):
    '''statement    :  RETURN expression'''
    t[0] = {}
    t[0]['nextList'] = [];## galat hai -- change karna hai
    quad = Quad('return',t[2]['place']);
    emit(quad);
                             
def p_statement(t):
    '''statement    :   designator  '''    ;
    if(debug2):
        print "statement goes to designator ", t[1]
    global stack
    global flag2
    #var = VarEntry();
    #arr = ArrayEntry();
    #proc = ProcEntry();
    
    symbolTable = stack[len(stack)-1]
    #if(type(symbolTable.LookUp(t[1]['name'])) == type(var) or type(symbolTable.LookUp(t[1]['name'])) == type(arr)):
    # TO FIX -- check if a function in a  module is called, it should be there. 
    if((t[1]['what']!= 'PROCEDURE') and (t[1]['what']!= 'MODULE')):
        print "ERROR : This must be a procedure designator" , " at line no ", t.lineno(1)
        flag2 = False 
    
    t[0] = {}
    t[0]['nextList'] = []    
    
#def p_statement_assign_function(t):
 #   '''statement :      designator checkConstant ASSIGN expression'''
def p_statement_assign(t):
    '''statement :       designator checkConstant ASSIGN expression'''
    global debug2
    global debug3
    global debug4
    global flag2;
    global targetList;
    if(debug2):
        print "Assignment!!!"
        print t[4]['place'];
        print t[1], t[4]

    t[0] = {'type':'VOID'}    
    tempQuad = None;
    global stack
    symbolTable = stack[len(stack)-1]
    if(t[1] and t[4]):
        if(t[1]['what']=='PROCEDURE'):
            print "ERROR: Invalid assignment statement" , " at line no ", t.lineno(3)
            
            flag2 = False
        
        else:            
            #if(t[1][0]!=t[4][0]):
            temp1 = t[1]['type']
            if(debug4):
                print t[4];
            temp2 = t[4]['type']
            dict = {};
            if(type(temp1) == type(dict)):
                temp1 = temp1['type']
            if(type(temp2) == type(dict)):
                temp2 = temp2['type']            
            if(temp1!=temp2):
                #if(t[1][0]=='REAL' and t[4][0]=='INTEGER'):
                if(temp1=='REAL' and temp2=='INTEGER'):
                    tempu = newTemp('REAL');
                    tempQuad = Quad('intToReal',t[4]['place'],None,tempu)
                    
                else:
                    #print "ERROR : Type mismatch" ,t[1]['type'] , t[4]['type'], " at line no ", t.lineno(3)
                    print "ERROR : Type mismatch" ,temp1 , temp2 , " at line no ", t.lineno(3)
                    flag2 = False
    
    if(t[1]['what']=='ARRAY' or t[1]['what']=='POINTER'):
        destPlace = 'lValue(' + str(t[1]['place']) + ')'
    else:
        destPlace = t[1]['place']       
    
    # code generation
    t[0]['code'] = t[4]['code'][:]
    
    if(tempQuad is None):             
        if(debug4):
            print " this should be defined for every assignment", t[4]['type']
        # might be an error
        testbool = False;
        try:
            if("'" in t[4]['place']):
                testbool = True;
        except:
            pass;
        if(testbool):
                quad = Quad(None, t[4]['place'], None, destPlace,'CHAR')
        else:
                #quad = Quad(None, t[4]['place'], None, destPlace, t[4]['type'])
                quad = Quad(None, t[4]['place'], None, destPlace)
        
            
    else:
        # might be an error
        quad = Quad(None, tempu, None, destPlace)   
        #quadList.append(tempQuad)
        emit(tempQuad);
        t[0]['code'].append(tempQuad)
    
    if(temp2 != 'BOOLEAN'):
        emit(quad);
        t[0]['code'].append(quad)
    else:
        backPatch(t[4]['trueList'], nextQuad())
        quad1 = Quad(None, True, None, destPlace)
        emit(quad1)
        quad2 = Quad('goto', None, None, nextQuad()+3);
        targetList.append(nextQuad()+3);
        emit(quad2)
        backPatch(t[4]['falseList'], nextQuad())
        quad3 = Quad(None, False, None, destPlace)
        emit(quad3)
        quad4 = Quad('goto', None, None, nextQuad()+1)
        targetList.append(nextQuad()+1);
        emit(quad4)

    t[0]['nextList'] = makeList_empty()
    if(t[4]['type'] == 'BOOLEAN'):
        if(debug4):
            print "assignmenet truelist ",t[4]['trueList']               
            print "assignment falselist",t[4]['falseList']               
            
def p_checkConstant(t):
    '''checkConstant : '''
    global stack
    global flag2
    symbolTable = stack[len(stack)-1]
    var = VarEntry()
    if(t[-1]):
        if(symbolTable.LookUp(t[-1]['name']) is None):
            print "ERROR : Variable assigned before declaration" , " at line no ", t.lineno(-1)

            flag2 = False
        
        elif(type(symbolTable.LookUp(t[-1]['name'])) == type(var)):
            
            if(symbolTable.LookUp(t[-1]['name']).constant):
                print "ERROR : Can't assign value to a constant" , " at line no ", t.lineno(-1)

                flag2 = False
            
        
    #dont evaluate expression

def p_statement_DEC(t):
    '''statement :    DEC '(' designator checkConstant ')'
                |    DEC '(' designator checkConstant ',' expression ')' '''
    global debug2
    global debug3
    global debug4
    global flag2;
    global targetList;
    if(debug2):
        print "builtIn Procedure!!!"
        print t[1], t[3]

    t[0] = {'type':'VOID'}    
    tempQuad = None;
    global stack
    symbolTable = stack[len(stack)-1]
    if(t[3]):
        if(t[3]['what']=='PROCEDURE' or t[3]['what']=='MODULE'):
            print "ERROR: Expected a variable for DEC. Procedure or Module found" , " at line no ", t.lineno(1)
            flag2 = False
        else:
            #if(t[1][0]!=t[4][0]):
            if(debug4):
                print t[3];
            temp2 = t[3]['type']
            dict = {};
            if(type(temp2) == type(dict)):
                temp2 = temp2['type']
            
            if(len(t)==8):
                temp3 = t[6]['type']
                dict = {};
                if(type(temp3) == type(dict)):
                    temp3 = temp3['type']
    
    if(temp2!='INTEGER' and temp2!='REAL' or (len(t)==8 and temp3!='INTEGER' and temp3!='REAL')):
        flag2 = False
        print "ERROR: Type mismatch. REAL or INTEGER expected for DEC at line no", t.lineno(1)
    if(len(t)==8 and flag2):
        if(temp2!=temp3):
            if(temp2=='INTEGER' and temp3=='REAL'):
                flag2 = False
                print "ERROR: Type mismatch. found REAL. INTEGER expected for DEC at line no", t.lineno(1)
    
    if(t[3]['what']=='ARRAY' or t[3]['what']=='POINTER'):
        destPlace = 'lValue(' + str(t[3]['place']) + ')'
        temp = newTemp(temp2)
        quad = Quad(None, 'rValue(' + str(t[3]['place']) + ')', None, temp)
        emit(quad)
        srcePlace = temp
    else:
        destPlace = t[3]['place']
        srcePlace = t[3]['place']
    
    if(len(t)==8 and flag2):
        if(temp2!=temp3):
            temp = newTemp(temp2)
            quad = Quad('intToReal', t[6]['place'], None, temp)
            emit(quad)
            decValue = temp
        else:
            decValue = t[6]['place']
    
    # code generation
    t[0]['code'] = t[3]['code'][:]
    if(flag2):
        if('(' in destPlace):
            if(temp2=='INTEGER'):
                if(len(t)==8):
                    temp = newTemp(temp2)
                    quad = Quad('-', srcePlace, decValue, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                else:
                    temp = newTemp(temp2)
                    quad = Quad('-', srcePlace, 1, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                emit(quad)
            else:
                if(len(t)==8):
                    temp = newTemp(temp2)
                    quad = Quad('-', srcePlace, decValue, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                else:
                    temp = newTemp(temp2)
                    quad = Quad('-', srcePlace, 1.0, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                emit(quad)
        else:
            if(temp2=='INTEGER'):
                if(len(t)==8):
                    quad = Quad('-', srcePlace, decValue, destPlace, temp2)
                else:
                    quad = Quad('-', srcePlace, 1, destPlace, temp2)
                emit(quad)
            else:
                if(len(t)==8):
                    quad = Quad('-', srcePlace, decValue, destPlace, temp2)
                else:
                    quad = Quad('-', srcePlace, 1.0, destPlace, temp2)
                emit(quad)
    t[0]['nextList'] = makeList_empty()

def p_statement_INC(t):
    '''statement :    INC '(' designator checkConstant ')'
                |    INC '(' designator checkConstant ',' expression ')' '''
    global debug2
    global debug3
    global debug4
    global flag2;
    global targetList;
    if(debug2):
        print "builtIn Procedure!!!"
        print t[1], t[3]

    t[0] = {'type':'VOID'}    
    tempQuad = None;
    global stack
    symbolTable = stack[len(stack)-1]
    if(t[3]):
        if(t[3]['what']=='PROCEDURE' or t[3]['what']=='MODULE'):
            print "ERROR: Expected a variable for INC. Procedure or Module found" , " at line no ", t.lineno(1)
            flag2 = False
        else:
            #if(t[1][0]!=t[4][0]):
            if(debug4):
                print t[3];
            temp2 = t[3]['type']
            dict = {};
            if(type(temp2) == type(dict)):
                temp2 = temp2['type']
            
            if(len(t)==8):
                temp3 = t[6]['type']
                dict = {};
                if(type(temp3) == type(dict)):
                    temp3 = temp3['type']
    
    if(temp2!='INTEGER' and temp2!='REAL' or (len(t)==8 and temp3!='INTEGER' and temp3!='REAL')):
        flag2 = False
        print "ERROR: Type mismatch. REAL or INTEGER expected for INC at line no", t.lineno(1)
    if(len(t)==8 and flag2):
        if(temp2!=temp3):
            if(temp2=='INTEGER' and temp3=='REAL'):
                flag2 = False
                print "ERROR: Type mismatch. found REAL. INTEGER expected for INC at line no", t.lineno(1)
    
    if(t[3]['what']=='ARRAY' or t[3]['what']=='POINTER'):
        destPlace = 'lValue(' + str(t[3]['place']) + ')'
        temp = newTemp(temp2)
        quad = Quad(None, 'rValue(' + str(t[3]['place']) + ')', None, temp)
        emit(quad)
        srcePlace = temp
    else:
        destPlace = t[3]['place']
        srcePlace = t[3]['place']
    
    if(len(t)==8 and flag2):
        if(temp2!=temp3):
            temp = newTemp(temp2)
            quad = Quad('intToReal', t[6]['place'], None, temp)
            emit(quad)
            decValue = temp
        else:
            decValue = t[6]['place']
    
    # code generation
    t[0]['code'] = t[3]['code'][:]
    if(flag2):
        if('(' in destPlace):
            if(temp2=='INTEGER'):
                if(len(t)==8):
                    temp = newTemp(temp2)
                    quad = Quad('+', srcePlace, decValue, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                else:
                    temp = newTemp(temp2)
                    quad = Quad('+', srcePlace, 1, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                emit(quad)
            else:
                if(len(t)==8):
                    temp = newTemp(temp2)
                    quad = Quad('+', srcePlace, decValue, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                else:
                    temp = newTemp(temp2)
                    quad = Quad('+', srcePlace, 1.0, temp, temp2)
                    emit(quad)
                    quad = Quad(None, temp, None, destPlace)
                emit(quad)
        else:
            if(temp2=='INTEGER'):
                if(len(t)==8):
                    quad = Quad('+', srcePlace, decValue, destPlace, temp2)
                else:
                    quad = Quad('+', srcePlace, 1, destPlace, temp2)
                emit(quad)
            else:
                if(len(t)==8):
                    quad = Quad('+', srcePlace, decValue, destPlace, temp2)
                else:
                    quad = Quad('+', srcePlace, 1.0, destPlace, temp2)
                emit(quad)
    t[0]['nextList'] = makeList_empty()

def p_builtIn_ABS(t):
    ''' builtIn    :    ABS '(' expression ')' '''
    global stack;
    global moduleDictionary;
    global debug2;
    global flag2
    global debug3;
    global targetList;
    if(debug2):
        print "builtin : ", t[1];
    symbolTable = stack[len(stack)-1];
    value = 0 
    #lookUp = symbolTable.LookUp(t[2])
    if(t[3]['type']!='REAL' and t[3]['type']!='INTEGER'):
        flag2 = False
        print "ERROR: INTEGER or REAL value expected for ABS at line no", t.lineno(1)
    else:
        t[0] = t[3]
        
        if(t[3]['value'] is not None):
            if(t[3]['value']>=0):
                value = t[3]['value']
            else:
                value = -t[3]['value']
            temp = newTemp(t[3]['type'])
            quad = Quad(None, value, None, temp, t[3]['type'])
            emit(quad)
            t[0]['value'] = value
        else:
            temp = newTemp(t[3]['type'])
            if(t[3]['type']=='INTEGER'):
                quad = Quad('<', t[3]['place'],0,nextQuad()+3, t[3]['type'])
                targetList.append(nextQuad()+3)
                emit(quad);
            else:
                quad = Quad('<', t[3]['place'],0.0,nextQuad()+3, t[3]['type'])
                targetList.append(nextQuad()+3)
                emit(quad);
            quad = Quad(None, t[3]['place'], None, temp, t[3]['type'])
            emit(quad)
            quad = Quad('goto',None, None, nextQuad()+2)
            targetList.append(nextQuad()+2)
            emit(quad)
            quad = Quad('UMINUS', t[3]['place'], None, temp, t[3]['type'])
            emit(quad)
            
        t[0]['place'] = temp
        #print "printing abs ", value
        #print t[3]

def p_builtIn_SIZE(t):
    ''' builtIn    :    SIZE '(' INTEGER ')'
                    |    SIZE '(' REAL ')'
                    |    SIZE '(' BOOLEAN ')'
                    |    SIZE '(' CHAR ')' ''';
    global debug2;
    global flag2
    global debug3;
    global intSize;
    global realSize;
    global charSize;
    global booleanSize;
    
    if(debug2):
        print "builtin : ", t[1];

    t[0] = {}
    value = 0
    if(t[3]=='INTEGER'):
        value = intSize
    elif(t[3]=='REAL'):
        value = realSize
    elif(t[3]=='CHAR'):
        value = charSize
    elif(t[3]=='BOOLEAN'):
        value = booleanSize
    else:
        flag2 = False
        print "ERROR: Basic data type expected for SIZE at line no", t.lineno(1)
    
    if(flag2):
        temp = newTemp('INTEGER')
        t[0]['place'] = temp
        t[0]['type'] = 'INTEGER'
        t[0]['value'] = value
        t[0]['code'] = []
        quad = Quad(None, value, None, temp, 'INTEGER')
        emit(quad)
        
def p_builtIn_MIN(t):
    ''' builtIn    :    MIN '(' INTEGER ')'
                    |    MIN '(' REAL ')'
                    |    MIN '(' BOOLEAN ')'
                    |    MIN '(' CHAR ')' ''';
    global debug2;
    global flag2
    global debug3;
    intMin = -2147483648;
    realMin = 0;
    charMin = 0;
    booleanMin = 0;
    
    if(debug2):
        print "builtin : ", t[1];

    t[0] = {}
    value = 0
    type = 'INTEGER'
    if(t[3]=='INTEGER'):
        value = intMin
    elif(t[3]=='REAL'):
        value = realMin
        type = 'REAL'
    elif(t[3]=='CHAR'):
        value = charMin
    elif(t[3]=='BOOLEAN'):
        value = booleanMin
    else:
        flag2 = False
        print "ERROR: Basic data type expected for MIN at line no", t.lineno(1)
    
    if(flag2):
        temp = newTemp(type)
        t[0]['place'] = temp
        t[0]['type'] = type
        t[0]['value'] = value
        t[0]['code'] = []
        quad = Quad(None, value, None, temp, type)
        emit(quad)

def p_builtIn_MAX(t):
    ''' builtIn    :    MAX '(' INTEGER ')'
                    |    MAX '(' REAL ')'
                    |    MAX '(' BOOLEAN ')'
                    |    MAX '(' CHAR ')' ''';
    global debug2;
    global flag2
    global debug3;
    intMax = 2147483647;
    realMax = 0;
    charMax = 255;
    booleanMax = 1;
    
    if(debug2):
        print "builtin : ", t[1];

    t[0] = {}
    value = 0
    type = 'INTEGER'
    if(t[3]=='INTEGER'):
        value = intMax
    elif(t[3]=='REAL'):
        value = realMax
        type = 'REAL'
    elif(t[3]=='CHAR'):
        value = charMax
    elif(t[3]=='BOOLEAN'):
        value = booleanMax
    else:
        flag2 = False
        print "ERROR: Basic data type expected for MIN at line no", t.lineno(1)
    
    if(flag2):
        temp = newTemp(type)
        t[0]['place'] = temp
        t[0]['type'] = type
        t[0]['value'] = value
        t[0]['code'] = []
        quad = Quad(None, value, None, temp, type)
        emit(quad)
        
def p_builtIn_CHR(t):
    ''' builtIn    :    CHR '(' expression ')' '''
    global debug2;
    global flag2
    global debug3;
    t[0] = {}
    if(t[3]['type']!='INTEGER'):
        flag2 = False
        print "ERROR: INTEGER expected. ", t[3]['type'], " found at line no. ", t.lineno(1)
    else:
        temp = newTemp('INTEGER')
        t[0]['place'] = temp
        t[0]['type'] = 'CHAR'
        t[0]['value'] = t[3]['value']
        t[0]['code'] = []
        quad = Quad(None, t[3]['place'], None, temp, 'INTEGER')
        emit(quad)
        
def p_builtIn_ORD(t):
    ''' builtIn    :    ORD '(' expression ')' '''
    global debug2;
    global debug4
    global flag2
    global debug3;
    t[0] = {}
    if(t[3]['type']!='CHAR'):
        flag2 = False
        print "ERROR: CHAR expected. ", t[3]['type'], " found at line no. ", t.lineno(1)
    else:
        temp = newTemp('CHAR')
        t[0]['place'] = temp
        t[0]['type'] = 'INTEGER'
        t[0]['value'] = t[3]['value']
        t[0]['code'] = []
        if("\'" in t[3]['place']):
            value = ord(t[3]['place'].split("\'")[1])
            if(debug4):
                print "Printing ord value ", value
        else:
            value = t[3]['place']
        quad = Quad(None, value, None, temp)
        emit(quad)        

def p_builtIn_CAP(t):
    ''' builtIn    :    CAP '(' expression ')' '''
    global debug2;
    global flag2
    global debug3;
    global targetList
    t[0] = {}
    if(t[3]['type']!='CHAR'):
        flag2 = False
        print "ERROR: CHAR expected. ", t[3]['type'], " found at line no. ", t.lineno(1)
    else:
        temp = newTemp('CHAR')
        t[0]['place'] = temp
        t[0]['type'] = 'CHAR'
        t[0]['value'] = t[3]['value']
        t[0]['code'] = []
        if("\'" in t[3]['place']):
            character = t[3]['place'].split("\'")[1]
            #print "Printing ord value ", value
            character = character.capitalize()
            value = ord(character)
        else:
            temp1 = newTemp('INTEGER')
            quad = Quad(None, t[3]['place'], None, temp1)
            emit(quad)
            quad1 = Quad('<', temp1, 97, nextQuad()+3, 'INTEGER')
            quad2 = Quad('>', temp1, 122, nextQuad()+3, 'INTEGER')
            targetList.append(nextQuad()+3)
            quad3 = Quad('-', temp1, 32, temp1, 'INTEGER')
            emit(quad1)
            emit(quad2)
            emit(quad3)
            value = temp1

        quad = Quad(None, value, None, temp)
        emit(quad)
            
                
def p_designator(t):    
    '''designator   :       ID  suffix
                    |    builtIn    ''';
    #Temporary solution -- without suffix consideration
    global stack;
    global moduleDictionary;
    global debug2;
    global flag2
    global debug3;
    global debug4
    if(debug2):
        print "Designator : ", t[1];
    var = VarEntry();
    proc = ProcEntry();
    arr = ArrayEntry();
    point = PointerEntry();
    toPass={};
    if(len(t)==2):
        t[0] = t[1];
        t[0]['what'] = 'BUILTIN'
    else:
        if(t[2] is not None):
            dict = {}
            if(type(t[2]) == type(dict)):
                
                toPass['place'] = t[2]['place']
                toPass['code'] = [];#t[2]['code']
        else:
            toPass['place'] = t[1]
            toPass['code'] = []
        
        symbolTable = stack[len(stack)-1]; 
        if(symbolTable.LookUp(t[1]) != None):
            # Variable
            if(type(symbolTable.LookUp(t[1])) == type(var)):
                relevantType = symbolTable.LookUp(t[1]).type
                toPass['name'] = t[1];
                toPass['type'] = relevantType;
                toPass['what'] = 'VARIABLE';
                #t[0] = (relevantType , t[1], 'VARIABLE');# instead of 0, pass the actual value OR null !!
            # Procedure    
            elif(type(symbolTable.LookUp(t[1])) == type(proc)):
                relevantType = symbolTable.LookUp(t[1]).type
                retType = symbolTable.LookUp(t[1]).returnType
                toPass['name'] = t[1];
                toPass['type'] = relevantType;
                toPass['what'] = 'PROCEDURE';
                toPass['returnType'] = retType;
                
                if(retType != 'VOID'):
                    h = newTemp(retType)     #
                    quad = Quad(None, 'retval', None, h);
                    emit(quad);
                    toPass['place'] = h;
                    
                    
                    #ERROR AA SAKTA HAI!!
            
            
            
            
            #t[0] = (relevantType , t[1], retType, 'PROCEDURE');
            # Array    
            elif(type(symbolTable.LookUp(t[1])) == type(arr)):
                #relevantType = symbolTable.LookUp(t[1]).type
                if(t[2] is not None):
                    relevantType = symbolTable.LookUp(t[1]).type
                    toPass['name'] = t[1];
                    toPass['type'] = relevantType;
                    toPass['what'] = 'ARRAY';
                    #t[0] = (relevantType , t[1], 'POINTER');
                else:
                    arrayDetail = symbolTable.LookUp(t[1])
                    temp = newTemp('INTEGER')
                    quad = Quad(None, arrayDetail.offset, None, temp)
                    #quadList.append(quad)
                    emit(quad);
                    toPass['code'].append(quad)
                    toPass['place'] = temp
                    toPass['name'] = t[1];
                    toPass['type'] = 'ARRAY';#DOUBT
                    toPass['what'] = 'POINTER';
                #toPass['name'] = t[1];
                #toPass['type'] = relevantType;
                #toPass['what'] = 'ARRAY';
                #toPass['code'] = t[2]['code'];
                #t[0] = (relevantType , t[1], 'ARRAY');
            # Pointer to array
            elif(type(symbolTable.LookUp(t[1])) == type(point)):
                if(t[2] is not None):
                    relevantType = symbolTable.LookUp(t[1]).type
                    toPass['name'] = t[1];
                    toPass['type'] = relevantType;
                    toPass['what'] = 'ARRAY';
                    #t[0] = (relevantType , t[1], 'POINTER');
                else:
                    arrayDetail = symbolTable.LookUp(t[1])
                    temp = newTemp('INTEGER')
                    quad = Quad(None, arrayDetail.offset, None, temp)
                    #quadList.append(quad)
                    emit(quad);
                    toPass['code'].append(quad)
                    toPass['place'] = temp
                    toPass['name'] = t[1];
                    toPass['type'] = 'ARRAY';#DOUBT
                    toPass['what'] = 'POINTER';
                    #t[0] = ('ARRAY', t[1], 'POINTER') #The POINTER attribute is introduced to just make the length compatible
    
        # TO FIX
        # DOUBT
                              
        elif(t[1] in moduleDictionary):
            toPass['name'] = t[1];
            toPass['type'] = 'MODULE';
            toPass['what'] = 'MODULE';
            t[0] = toPass
            # check for length of t[2]
            returnTypesOfModule =  moduleReturnTypes[moduleDictionary[t[1]]];
            #h = newTemp()     #
            #quad = Quad(None, 'retval', None, h);
            #emit(quad);
                
        else:
            toPass['type'] = 'ERROR';
            toPass['name'] = t[1];
            toPass['what'] = 'ERROR';
            t[0] = toPass
            print "ERROR: in designator !!" , " at line no ", t.lineno(1)
            flag2 = False
        t[0] = toPass
        myType = t[0]['type']
        if(debug4):
            print "Printing boolean ", t[1]
        dict = {}
        while(type(myType)==type(dict)):
            myType = myType['type'] 
        #print "snjkbfkj",t[0]['type'], t[1], myType
        #EXTRA FEATURE: If one wants to follow semantics of C with respect to pointers and arrays, then hust remove the following if
        if(myType=='ARRAY' or myType=='POINTER'): #check for POINTER might not be necessary
            print "ERROR: Expression not compatible with variable type ARRAY or POINTER at line no.", t.lineno(1) 
            flag2 = False
     
        
def p_suffix(t):
    '''suffix :      '.' ID   suffix
    | '[' optionalExpressionList ']'  suffix
    | '^'  suffix
    | '(' optionalExpressionList ')' suffix  
    |    '''
    global stack
    global moduleDictionary
    global moduleList
    global flag2
    global debug2
    global debug3
    global debug4
    global intSize, realSize ,charSize, booleanSize, pointerSize
    
    symbolTable = stack[len(stack)-1]
    #Dealing with a[i]s
    """if(len(t)>1 and t[1]=='[' and t[-1]!='^'):
        arrayDetail = symbolTable.LookUp(t[-1])
        print "Array ", t[-1]
        if(t[2] is None):
            print "ERROR: array index missing"
        elif(len(t[2])>1):
            print "ERROR: invalid array index"
        elif(t[2][0]!='INTEGER'):
            print "ERROR: invalid array index"
        else:
            t[0] = arrayDetail.type
            print "Valid array index", t[0]"""
    #t[0]={};
    #t[0]['code'] = [];
         
    if(len(t)==5):
        oldt2 = t[2];
        if(debug4):
            print "oldt2 "    , oldt2;    
        newt2=[];
        for each in t[2]:
            newt2.append(each['type']);        
        t[2] = newt2;
        if(debug4):
            print "newt2" , newt2;
    
    if(len(t)>1 and t[1]=='^'):
        #if(t[2] is None):
            #print "ERROR: Expression not compatible with variable type array"
        t[0] = t[2];
                
    if(len(t)>1 and t[1]=='['):
        if(t[-1]=='^'):
            i = -2;
        else:
            i = -1;
        arrayDetail = symbolTable.LookUp(t[i])
        #print "type of entry", type(arrayDetail);
        if(debug2):
            print "Array ", t[i]
        if(t[2] is None):
            print "ERROR: array index missing" , " at line no ", t.lineno(1)
            flag2 = False
        
        elif(len(t[2])>1):# one dimensional array
            print "ERROR: invalid array index" , " at line no ", t.lineno(1)
            flag2 = False
        
        elif(t[2][0]!='INTEGER'):
            print "ERROR: invalid array index" , " at line no ", t.lineno(1)
            flag2 = False
        
        else:
            # TO SET CODE
            t[0] = arrayDetail.type # t[0] is a dictionary !
            arrLen = arrayDetail.length;
            arrType = arrayDetail.type['type']
            if(arrType=='INTEGER'):
                width = intSize;
            elif(arrType=='REAL'):
                width = realSize;
            elif(arrType=='CHAR'):
                width = charSize;
            elif(arrType=='BOOLEAN'):
                width = booleanSize;
            
            parentOffset = arrayDetail.offset
            #myOffset = parentOffset + width*oldt2[0]['place']
            if(i==-2):
                parentOffset += pointerSize;
            
            temp1 = newTemp('INTEGER');
            quad1 = Quad('*',oldt2[0]['place'], width, temp1, 'INTEGER');
            #quadList.append(quad1);
            emit(quad1);
            if(debug3):
                print "Quad 1 :", quad1.Print();
            
            temp2 = newTemp('INTEGER');
            quad2 = Quad('+',parentOffset, temp1 , temp2, 'INTEGER');
            #quadList.append(quad2);
            emit(quad2);    
            if(debug3):
                print "Quad 2 :", quad2.Print();    
                print "oldt2 place ", oldt2[0]['place']
            
            t[0]['code'] = [quad1,quad2];
            t[0]['place'] = temp2;
            if(debug2):
                print "Valid array index", t[0]
        
    #when we are calling function call with parameters
    if(len(t)>1 and t[1]=='('):
        #function call
        if(debug2):
            print "function call"
            print "suffix", t[-1], t[-3]
        if(symbolTable.LookUp(t[-1])):              #lookup func name in symbol table
            '''typeList = []
            print "optionalexpressionlist ", t[2]
            for i in t[2]:
                typeList.append(i[0])
            print "typelist ", typeList'''
            typeList = t[2]
            if(debug2):
                print "typelist ", typeList
            if(symbolTable.LookUp(t[-1]).type==typeList):
                t[0] = symbolTable.LookUp(t[-1]).returnType
                if(debug2):
                    print "Printing return type", t[0]
                #pass
                #Here we are not checking whether symbolTable.LookUp(t[-1]) is an object of ProcEntry or not
            elif(len(symbolTable.LookUp(t[-1]).type)>len(typeList)):
                print "ERROR : Too few arguments" , " at line no ", t.lineno(1)

                flag2 = False
        
            elif(len(symbolTable.LookUp(t[-1]).type)<len(typeList)):
                print "ERROR : Too many arguments" , " at line no ", t.lineno(1)

                flag2 = False
        
            else:
                print "ERROR : Argument not compatible with formal parameters" , " at line no ", t.lineno(1)

                flag2 = False
            
            funcName = t[-1]
            
        else:
            try: 
                funcName = moduleDictionary[t[-3]] + "." + t[-1]
            except KeyError:
                funcName = t[-3] + "." + t[-1]
                
            if(debug4):
                print "Expecting module"
                print "funcName = ", funcName;
            
            if(t[-3] in moduleDictionary):          #lookup func name in module list of the module, i.e., if Out.hello() is called, t[-3] is Out
                if(t[-1] in moduleList[moduleDictionary[t[-3]]]):
                    #check the expressionlist
                    typeList = t[2]
                    tempmodule = moduleDictionary[t[-3]];
                    typeList2 = moduleExpList[tempmodule][t[-1]]
                    if(typeList == typeList2):
                        pass
                    elif(len(typeList) < len( typeList2 )):
                        print "ERROR : Too few arguments" , " at line no ", t.lineno(1)

                        flag2 = False
                    elif(len(typeList) > len( typeList2 )):
                        print "ERROR : Too many arguments" , " at line no ", t.lineno(1)

                        flag2 = False
                    else:
                        print "ERROR : Argument not compatible with formal parameters" , " at line no ", t.lineno(1)

                        flag2 = False
            
              
                else:
                    print "ERROR : No such function defined in module" , " at line no ", t.lineno(1)

                    flag2 = False
            
             
            
            
            else:
                print "ERROR: function called before declaration" , " at line no ", t.lineno(1)

                flag2 = False
        
        #function call code generation
        if(debug4):
            print "printing expression list ", t[2]
        #oldt2.reverse()
        if(funcName == 'Out.String'):
            global stringList;
            stringList.append(oldt2[0]['place']);
            quad = Quad('param', len(stringList) , None, None, 'STRING');
            emit(quad)
            quad = Quad('call', funcName, 1)
            emit(quad)
        else:
            jcount = 0
            for i in oldt2:
                quad = Quad('param', i['place'])
                emit(quad)
                jcount+=1
            quad = Quad('call', funcName, jcount)
            emit(quad)
            
    if(len(t)>1 and t[1]=='.'):
        if(debug2):
            print "hellooooooooo", t[2], t[-1]
    
    #when we are calling add2; or Out.add2; i.e., function call without parameters
    proc = ProcEntry()
    if(len(t)==1):
        if(debug2):
            print t[-1]
        if(t[-1]!=')' and t[-1]!=']' and t[-1]!='^'):
            if(symbolTable.LookUp(t[-1])):
                if(type(symbolTable.LookUp(t[-1])) == type(proc)):
                    if(debug2):
                        print "function" ,t[-1]," called without parameters"
                    
                    quad = Quad('call', t[-1])
                    emit(quad)
                    
                    if(symbolTable.LookUp(t[-1])):              
                        if(len(symbolTable.LookUp(t[-1]).type)==0):
                            pass
                        else:
                            print "ERROR : Too few arguments" , " at line no ", t.lineno(1)

                            flag2 = False
        
                        
            else:
                if(t[-3] in moduleDictionary):
                    if(t[-1] in moduleList[moduleDictionary[t[-3]]]):
                        quad = Quad('call', t[-3]+"."+t[-1])
                        emit(quad)
                    else:
                        print "ERROR : No such function defined in module" , " at line no ", t.lineno(-1)

                        flag2 = False

                else:
                    print "ERROR: function called before declaration" , " at line no ", t.lineno(-1)

                    flag2 = False
                    
                
                
                    

    if(len(t)==1):
        if(debug4):
            print "Empty Suffix";
    else:
        if(debug4):
            print "Suffix t[0] passed" , t[0];
    
def p_optionalExpressionList(t):    
    '''optionalExpressionList     :       expressionList 
    |    ''';
    if(len(t)>1):
        if(debug2):
            print "printing reverse", t[1].reverse()
        t[0] = t[1]

        
# Broadly, The specification that follow are responsible for the correct parsing of the Control Statements of IF and CASE


        

        

# Broadly, The following specification shall be used in the parsing of the expressions
# Expressions in general consist of many terms separated by addition type operators 
# Terms consist of many different factors separated by multiplicaiton type operators

def p_qualident(t): #The different Identifiers 
    '''qualident    :       ID 
    | ID  '.' ID 
    | INTEGER
    | CHAR
    | REAL  
    | BOOLEAN ''';
    global debug2;
    if(len(t)==2):                      #assign value to types
        t[0] = t[1];
    else:
        t[0] = t[1]+t[2]+t[3]
        if(debug2):
            print("Found Qualident as ID.ID");

def p_type(t):
    '''type         :       qualident  ''';
    #Assign value to types
    global flag2;
    toPass = {};
    toPass['type'] = t[1];
    if(t[1]!='INTEGER' and t[1]!= 'REAL' and t[1]!= 'CHAR' and t[1]!='BOOLEAN'):
        print "ERROR: redefination of types not allowed, at line no", t.lineno(1);
        flag2=False;
    toPass['category'] = 'SIMPLE';
    #t[0] = t[1]
    t[0] = toPass;
    
        
        
def p_type_array(t):
    '''type         : ARRAY expression OF type'''
    global flag2;
    if(t[2] is None):
        print "ERROR: Invalid array size" , " at line no ", t.lineno(1);
        flag2 = False
        result()
        
    if(t[2]['type']!='INTEGER'):                                        #handling only 1 D array
        print "ERROR : Invalid array size" , " at line no ", t.lineno(1);
        flag2 = False    
    else:
        #if(t[2][1]!=0):
        #t[0] = ("ARRAY" , t[2]['value'], t[4])            # array , its length , its type    
        toPass = {};
        toPass['type'] = 'ARRAY'
        toPass['arrayLength'] = t[2]['value']
        toPass['arrayOf'] = t[4]
        toPass['category'] = 'COMPLEX';
        t[0] = toPass;
        
def p_type_pointer(t):
    '''type : POINTER TO ARRAY expression OF type'''
    #t[0] = ( "POINTER" ,t[4]['value'] ,t[6]) ; # pointer , pointing to array of length , type
    toPass = {};
    toPass['type'] = 'POINTER';
    toPass['arrayLength'] = t[4]['value'];
    toPass['arrayOf'] = t[6];
    toPass['category'] = 'COMPLEX'
    t[0] = toPass;
    
def p_expressionList(t):
    '''expressionList :      expression ',' expressionList
    | expression    '''
    
    if(len(t)==2):
        #t[0] = [t[1]['type']]
        t[0] = [t[1]]
        if(debug2):
            print "Expression list 2 ", t[0]
    else:
        if(debug2):
            print "Expression list 3", t[3]
            print "Expression list 1", t[1]['type']
        #t[3].append(t[1][0])
        t[3].append(t[1])
        t[0] = t[3]
        if(debug2):
            print "Expression list 4 ", t[0]
            

def p_expression_u(t):
    '''expression :  '-' expression %prec UMINUS
                    | '+' expression %prec UPLUS'''
    
    t[0] = {};
    global debug2
    global debug4
    # Doubtful fix
    #print "Printing t2", t[2]
    '''
    temp = newTemp(t[2]['type'])
    t[0]['place'] = temp
    if(t[1]=='-'):
        quad = Quad('UMINUS',t[2]['place'], None, temp, t[2]['type'])
        print "Printing Uminus", t[2]['place'], temp
    else:
        quad = Quad('UPLUS',t[2]['place'], None, temp, t[2]['type'])
        print "Printing Uplus", t[2]['place'], temp
    #quadList.append(quad)
    emit(quad);
    t[0]['code'] = t[2]['code'][:]
    t[0]['code'].append(quad)
    #t[0]['code'] = t[2]['code'] + temp + ":= -" + t[2]['place']
    '''
    
    if(debug2):
        print "UNARY"
    myType = t[2]['type']
    dict = {}
    while(type(myType)==type(dict)): #if might not suffice here
        myType = myType['type']

    if(myType=='INTEGER' or myType=='REAL'):
        
        t[0]['type'] = myType
        if(t[2]['value'] is not None):
            if(t[1]=='+'):
                t[0]['value'] = t[2]['value'];
            else:
                t[0]['value'] = - t[2]['value'];
            t[0]['place'] = t[0]['value']
            t[0]['code'] = []
        else:
            temp = newTemp(t[2]['type'])
            t[0]['place'] = temp
            if(t[1]=='-'):
                quad = Quad('UMINUS',t[2]['place'], None, temp, t[2]['type'])
                if(debug4):
                    print "Printing Uminus", t[2]['place'], temp
            else:
                quad = Quad('UPLUS',t[2]['place'], None, temp, t[2]['type'])
                if(debug4):
                    print "Printing Uplus", t[2]['place'], temp
            #quadList.append(quad)
            emit(quad);
            t[0]['code'] = t[2]['code'][:]
            t[0]['code'].append(quad)
            #t[0]['code'] = t[2]['code'] + temp + ":= -" + t[2]['place']
            t[0]['value'] = None
    else:
        print "ERROR : Incompatible type for unary operator" , " at line no ", t.lineno(1), myType, 'found'
        global flag2
        flag2 = False


def p_expression_factor_conversion(t):
    '''expression         : factor'''
    global debug2
    global debug3
    global debug4
    if not (t[1] == None):
        if(debug2):
            print t[1];
            print t[1]['type'];
            print t[1]['value'];
        #t[0] = [t[1]['type'],t[1]['value']];
    
    toPass = {};
    toPass['type'] = t[1]['type'];
    toPass['value'] = t[1]['value'];
    toPass['code'] = t[1]['code'];
    if(t[1]['what']=='ARRAY'):
        temp = newTemp(t[1]['type']['type'])
        quad = Quad(None, 'rValue('+ str(t[1]['place'] + ')'), None, temp)
        emit(quad)
        #toPass['place'] = 'rValue('+ str(t[1]['place'] + ')');
        toPass['place'] = temp
    elif(t[1]['what']=='POINTER'):
        toPass['place'] = 'lValue('+ str(t[1]['place'] + ')');
    else:
        toPass['place'] = t[1]['place']
    '''
    if(toPass['type'] == 'BOOLEAN'):
        toPass['trueList'] = makeList(nextQuad())#t[1]['trueList'];
        toPass['falseList'] = makeList(nextQuad()+1)#t[1]['falseList'];
        quad = Quad('ifgoto', toPass['place'], None, None)
        emit(quad)
        quad = Quad('goto', None, None, None)
        emit(quad)
    '''
    
    if(t[1]['type'] == 'BOOLEAN'):
        if(debug4):
            print "printing t[1] ", t[1]
        toPass['trueList'] = t[1]['trueList']
        toPass['falseList'] = t[1]['falseList']
    
    t[0] = toPass;  

def p_boolMarker(t):
    '''boolMarker : '''
    t[0] = {}
    t[0]['quad'] = nextQuad()
    
def p_nMarker(t):
    '''nMarker : '''
    t[0] = {};
    t[0]['nextList'] = makeList(nextQuad())
    quad = Quad('goto')
    emit(quad)

def p_expression_bool(t):
    '''expression    :   expression OR boolMarker expression
    | expression '&' boolMarker expression
    '''
    var = None;  
    t[0]={};
    t[0]['trueList']=[];
    t[0]['falseList']=[];
    global debug2;
    global debug3;
    global debug4
    global flag2;
    
    if(len(t) > 2):
        t[0]['place'] = newTemp('BOOLEAN')
        if(debug3):
            print "t4 code"
            for i in t[4]['code']:
                i.Print()
            print "t1 code"
            for i in t[1]['code']:
                i.Print()
            
        if(t[1]['value'] is not None):    
            temp1 = t[1]['value']
        else:
            temp1 = t[1]['place']
        if(t[4]['value'] is not None):    
            temp4 = t[4]['value']
        else:
            temp4 = t[4]['place']

        if(t[1]['value'] is not None and t[4]['value'] is not None):
            if(t[2]=='OR'):
                var = temp1 or temp4
            elif(t[2]=='&'):
                var = temp1 and temp4
                
            
        
        dict = {}
        myType1 = t[1]['type'];
        myType4 = t[4]['type'];
        while(type(myType1)==type(dict)):
            #if(type(myType1)==type(dict)):    
            myType1 = myType1['type']
        
        while(type(myType4)==type(dict)):
            #if(type(myType4)==type(dict)):
            myType4 = myType4['type']
            
        
    
        if(debug2):
            if(t[2] == '&'):
                print "doing AND!!!!"
            elif(t[2] == 'OR'):
                print "doing OR!!!!"
            print t[1];
            print t[4];
        
        if(myType1=='BOOLEAN' and myType4 == 'BOOLEAN'):
            t[0]['type'] = 'BOOLEAN'
            if(t[1]['value'] is not None and t[4]['value'] is not None):
                if(t[2]=='&'):
                    t[0]['value'] = t[1]['value'] and t[4]['value']
                else:
                    t[0]['value'] = t[1]['value'] or t[4]['value']
            else:
                t[0]['value'] = None
                #t[0] = ('BOOLEAN', 0)
        elif(myType1!='BOOLEAN' and myType4 != 'BOOLEAN'):
            if(debug4):
                print myType1, " kdf ", myType4
            print "ERROR : Invalid type for operator" , " at line no ", t.lineno(2)

            flag2 = False
    
        else:
            print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)

            flag2 = False                    
    
        if(var is not None):
            quad = Quad(None, var , None, t[0]['place'])
        else:
            quad = Quad(t[2], temp1, temp4, t[0]['place'])
        #quadList.append(quad)
        emit(quad);
        
        newQuadList = []
        newQuadList = t[1]['code'][:].extend(t[4]['code'][:])
        if(newQuadList is None):
            newQuadList = []
        #print newQuadList
        newQuadList.append(quad)
        t[0]['code'] = newQuadList
        
    if(t[2] == 'OR'):            
        backPatch(t[1]['falseList'], t[3]['quad'])
        t[0]['trueList'] = merge(t[1]['trueList'], t[4]['trueList'])
        t[0]['falseList'] = t[4]['falseList']
    elif(t[2] == '&'):            
        backPatch(t[1]['trueList'], t[3]['quad'])
        t[0]['falseList'] = merge(t[1]['falseList'], t[4]['falseList'])
        t[0]['trueList'] = t[4]['trueList']
                
    if(debug4):
        print t[1] , t[4]        
        print "t[0] passed " , t[0];
    
def p_expression_relational(t):
    '''expression :  expression '#' expression
    | expression '=' expression
    | expression '<' expression
    | expression LESSEQUAL expression
    | expression '>' expression
    | expression MOREEQUAL expression
    '''  
    t[0]={};
    t[0]['trueList'] = [];
    t[0]['falseList'] = [];
    t[0]['type'] = 'BOOLEAN'
    
    global debug2;
    global debug3;
    global debug4
    global flag2;
    var = None;
    tempQuad = None;
    opType = None;
    if(len(t) > 2):
        if(debug3):
            print "t3 code"
            for i in t[3]['code']:
                i.Print()
            print "t1 code"
            for i in t[1]['code']:
                i.Print()            
        if(t[1]['value'] is not None):    
            temp1 = t[1]['value']
        else:
            temp1 = t[1]['place']
        if(t[3]['value'] is not None):    
            temp3 = t[3]['value']
        else:
            temp3 = t[3]['place']        
        dict = {}
        myType1 = t[1]['type'];
        myType3 = t[3]['type'];
        while(type(myType1)==type(dict)):
            myType1 = myType1['type']
        while(type(myType3)==type(dict)):
            myType3 = myType3['type']
            
        if(t[2]=='<' or t[2]=='>' or t[2]=='<=' or t[2]=='>='):
            if(debug2):
                if(t[2]=='<'):
                    print "doing LESS THAN!!!!"
                elif(t[2]=='>'):
                    print "doing GREATER THAN!!!!"
                elif(t[2]=='<='):
                    print "doing LESS THAN EQUAL!!!!"
                elif(t[2]=='>='):
                    print "doing GREATER THAN EQUAL!!!!"
                print t[1];
                print t[3];
            
            if(myType1=='INTEGER'):
                if(debug4):
                    print "type1 is int"
                if(myType3=='INTEGER' or myType3=='REAL'):
                    t[0]['type'] = 'BOOLEAN'
                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[2]=='<'):
                            t[0]['value'] = t[1]['value'] < t[3]['value']
                        elif(t[2]=='>'):
                            t[0]['value'] = t[1]['value'] > t[3]['value']
                        elif(t[2]=='<='):
                            t[0]['value'] = t[1]['value'] <= t[3]['value']
                        elif(t[2]=='>='):
                            t[0]['value'] = t[1]['value'] >= t[3]['value']
                    else:                        
                        t[0]['value'] = None
                        if(myType3=='REAL'):
                                if(debug4):
                                    print "type3 is real"
                                tempu = newTemp('REAL');
                                tempQuad = Quad('intToReal',temp1,None,tempu);
                                temp1 = tempu
                                opType = 'REAL'
                        else:
                            if(debug4):
                                print "type3 is int"
                            opType = 'INTEGER'
                        
                            
                if(myType3=='BOOLEAN' or myType3=='CHAR'):
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False

            if(myType1=='REAL'):
                if(myType3=='INTEGER' or myType3=='REAL'):
                    opType = 'REAL'
                    t[0]['type'] = 'BOOLEAN'
                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[2]=='<'):
                            t[0]['value'] = t[1]['value'] < t[3]['value']
                        elif(t[2]=='>'):
                            t[0]['value'] = t[1]['value'] > t[3]['value']
                        elif(t[2]=='<='):
                            t[0]['value'] = t[1]['value'] <= t[3]['value']
                        elif(t[2]=='>='):
                            t[0]['value'] = t[1]['value'] >= t[3]['value']
                    else:                        
                        t[0]['value'] = None
                        if(myType3=='INTEGER'):
                                tempu = newTemp('REAL');
                                tempQuad = Quad('intToReal',temp3,None,tempu);
                                temp3 = tempu

                if(myType3=='BOOLEAN' or myType3=='CHAR'):
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
                    
            if(myType1=='BOOLEAN'):
                if(myType3=='BOOLEAN'):
                    print "ERROR : Invalid type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
        
                else:
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
        
            if(myType1=='CHAR'):
                if(myType3=='CHAR'):
                    t[0]['type'] = 'BOOLEAN'
                    t[0]['value'] = None
                    opType = 'CHAR'
                    #t[0] = ('BOOLEAN',0)
                else:
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
     
        elif(t[2] == '=' or t[2] == '#'):
            if(debug2):
                if(t[2] == '='):
                    print "doing EQUAL!!!!"
                elif(t[2] == '#'):
                    print "doing NOT EQUAL!!!!"
                print t[1];
                print t[3];
                
            if(myType1=='INTEGER'):
                if(myType3=='INTEGER' or myType3=='REAL'):
                    t[0]['type'] = 'BOOLEAN'

                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[2]=='='):
                            t[0]['value'] = t[1]['value'] == t[3]['value']
                        elif(t[2]=='#'):
                            t[0]['value'] = t[1]['value'] != t[3]['value']
                    else:                        
                        t[0]['value'] = None
                        if(myType3=='REAL'):
                                tempu = newTemp('REAL');
                                tempQuad = Quad('intToReal',temp1,None,tempu);
                                temp1 = tempu
                                opType = 'REAL'
                        else:
                            opType = 'INTEGER'

                if(myType3=='BOOLEAN' or myType3=='CHAR'):
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
        
            if(myType1=='REAL'):
                if(myType3=='INTEGER' or myType3=='REAL'):
                    opType = 'REAL'
                    t[0]['type'] = 'BOOLEAN'

                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[2]=='='):
                            t[0]['value'] = t[1]['value'] == t[3]['value']
                        elif(t[2]=='#'):
                            t[0]['value'] = t[1]['value'] != t[3]['value']
                    else:                        
                        t[0]['value'] = None
                        if(myType3=='INTEGER'):
                                tempu = newTemp('REAL');
                                tempQuad = Quad('intToReal',temp3,None,tempu);
                                temp3 = tempu

                if(myType3=='BOOLEAN' or myType3=='CHAR'):
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
                    
            if(myType1=='BOOLEAN'):
                if(myType3=='BOOLEAN'):
                    t[0]['type'] = 'BOOLEAN'
                    opType = 'BOOLEAN'
                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[2]=='='):
                            t[0]['value'] = t[1]['value'] == t[3]['value']
                        elif(t[2]=='#'):
                            t[0]['value'] = t[1]['value'] != t[3]['value']  
                    else:                        
                        t[0]['value'] = None
                        #t[0] = ('BOOLEAN', 0)

                else:
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
        
            if(myType1=='CHAR'):
                if(myType3=='CHAR'):
                    t[0]['type'] = 'BOOLEAN'
                    opType = 'BOOLEAN'
                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[2]=='='):
                            t[0]['value'] = t[1]['value'] == t[3]['value']
                        elif(t[2]=='#'):
                            t[0]['value'] = t[1]['value'] != t[3]['value']
                    else:                        
                        t[0]['value'] = None
                        #t[0] = ('BOOLEAN', 0)

                else:
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
        
        # Intermediate code generation
        newQuadList = []
        #print "opType is", opType
        if(tempQuad is not None):
            #quadList.append(tempQuad)
            emit(tempQuad);
            newQuadList.append(tempQuad);
        var = t[0]['value'];
        #print "value of var is", var
        result = newTemp('BOOLEAN') #This result is used in the  "This should never be displayed!" else
        t[0]['place'] = result
        if(var is not None):
            quad = Quad(None, var , None, result)
            emit(quad);
            
            t[0]['trueList'] = []
            t[0]['falseList'] = []
        else:
            if(opType is not None):
                #print "Successful", opType
                quad = Quad(t[2], temp1, temp3, result, opType)
            else:
                quad = Quad(t[2], temp1, temp3, result)  
            #NOTE : this quad is never emitted          
        newQuadList = t[1]['code'][:].extend(t[3]['code'][:])
        if(newQuadList is None):
            newQuadList = []
        newQuadList.append(quad)
        t[0]['code'] = newQuadList    
    t[0]['trueList'] = makeList(nextQuad())
    t[0]['falseList'] = makeList(nextQuad()+1)
    if(opType is not None):
        quad1 = Quad(t[2], temp1, temp3, None, opType)# if with unspecified goto        
    else:
        #print "This should never be displayed!"; This infact will be executed and needed also
        quad1 = Quad('ifgoto', result, None, None, 'evaluatedBOOLEAN')# if with unspecified goto
        #The following was the being emitted previously (till 10th April)
        #quad1 = Quad(t[2], temp1, temp3)# if with unspecified goto
    quad2 = Quad('goto')
    emit(quad1)
    emit(quad2)
    if(debug4):
        print t[1] , t[3]        
        print "t[0] passed " , t[0];
            
def p_expression(t):
    '''expression         :       expression '+' expression
    | expression '-' expression
    | expression '*' expression
    | expression '/' expression
    | expression DIV expression
    | expression MOD expression
    
    ''';
    global stack
    symbolTable = stack[len(stack)-1]
    
    t[0]={};
    global debug2;
    global debug3;
    global debug4
    global flag2;
    var = None;
    tempQuad = None;
    opType = None;
    if(len(t) > 2):
        
        if(debug3):
            print "t3 code"
            for i in t[3]['code']:
                i.Print()
            print "t1 code"
            for i in t[1]['code']:
                i.Print()
            
        if(t[1]['value'] is not None):    
            temp1 = t[1]['value']
        else:
            temp1 = t[1]['place']
        if(t[3]['value'] is not None):    
            temp3 = t[3]['value']
        else:
            temp3 = t[3]['place']

        
        dict = {}
        myType1 = t[1]['type'];
        myType3 = t[3]['type'];
        while(type(myType1)==type(dict)):
            myType1 = myType1['type']
        while(type(myType3)==type(dict)):
            myType3 = myType3['type']
            

        if(t[2]=='+' or t[2]=='-' or t[2]=='*' or t[2]=='/' or t[2]=='MOD' or t[2]=='DIV'):
            if(debug2):
                if(t[2] == '+'):
                    print "doing ADDITION!!!!"
                elif(t[2] == '-'):
                    print "doing SUBTRACTION!!!!"
                elif(t[2] == '*'):
                    print "doing MULTIPLICATION!!!!"
                elif(t[2] == '/'):
                    print "doing DIVISION!!!!"
                elif(t[2] == 'MOD'):
                    print "doing MODDING!!!!"
                elif(t[2] == 'DIV'):
                    print "doing DIVVING!!!!"            
                print t[1];
                print t[3];

            if(myType1=='INTEGER'):
                if(myType3=='INTEGER'):
                    t[0]['type'] = 'INTEGER';
                    opType = 'INTEGER'; # because of different instructions
                    if(t[1]['value'] is not None and t[3]['value'] is not None):
                        if(t[3]['value']!=0 or (t[2]!='/' and t[2]!='MOD' and t[2]!='DIV')): 
                            
                            if(t[2] == '+'):
                                t[0]['value'] = t[1]['value']+t[3]['value']
                                
                            elif(t[2] == '-'):
                                t[0]['value'] = t[1]['value']-t[3]['value']
                                
                            elif(t[2] == '*'):
                                t[0]['value'] = t[1]['value']*t[3]['value']
                                
                            elif(t[2] == '/'):
                                t[0]['value'] = float(t[1]['value'])/t[3]['value']
                                
                                t[0]['type'] = 'REAL'
                            elif(t[2] == 'MOD'):
                                t[0]['value'] = t[1]['value']%t[3]['value']
                                
                            elif(t[2] == 'DIV'):
                                t[0]['value'] = t[1]['value']/t[3]['value']
                            
                                
                        else:
                            print "ERROR : division by 0 exception" , " at line no ", t.lineno(2)
                            
                            flag2 = False

                            if(t[2]=='/'):
                                t[0]['type'] = 'REAL'
                            t[0]['value'] = None       
                    else:
                        if(t[2]=='/'):
                            t[0]['type'] = 'REAL';
                            
                            tempu = newTemp('REAL');
                            tempQuad = Quad('intToReal',temp1,None,tempu);
                            temp1 = tempu
                            
                            emit(tempQuad);
                            
                            tempv = newTemp('REAL');
                            tempQuad = Quad('intToReal',temp3,None,tempv);
                            temp3 = tempv
                            
                            opType = 'REAL';
                                                    
                        t[0]['value'] = None;
                        
                        
                        

                if(myType3=='REAL'):
                    if(t[2]=='MOD' or t[2]=='DIV'):
                        print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                        
                        flag2 = False
                    else:
                        t[0]['type'] = 'REAL';
                        opType = 'REAL'; # because of different instructions
                        if(t[1]['value'] is not None and t[3]['value'] is not None):                    
                            if(t[3]['value']!=0 or t[2]!='/'): 
                                if(t[2] == '+'):
                                    t[0]['value'] = t[1]['value']+t[3]['value']
                                elif(t[2] == '-'):
                                    t[0]['value'] = t[1]['value']-t[3]['value']
                                elif(t[2] == '*'):
                                    t[0]['value'] = t[1]['value']*t[3]['value']
                                elif(t[2] == '/'):
                                    t[0]['value'] = float(t[1]['value'])/t[3]['value']
                                    t[0]['type'] = 'REAL'
                            else:
                                print "ERROR : division by 0 exception" , " at line no ", t.lineno(2) 
                                flag2 = False                   
                                t[0]['value'] = None 
                        else:
                            t[0]['value'] = None;
                            
                            
                            tempu = newTemp('REAL');
                            tempQuad = Quad('intToReal',temp1,None,tempu);
                            temp1 = tempu
                            
                            
                                                
                if(myType3=='BOOLEAN' or myType3=='CHAR'):
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False

            if(myType1=='REAL'):
                if(t[2]=='MOD' or t[2]=='DIV'):
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
                    
                    flag2 = False
                else:
                    if(myType3=='INTEGER' or myType3=='REAL'):
                        t[0]['type'] = 'REAL';
                        opType = 'REAL'; # because of different instructions
                        if(t[1]['value'] is not None and t[3]['value'] is not None):
                            if(t[2]!='/' or t[3]['value']!=0): 
                                if(t[2] == '+'):
                                    t[0]['value'] = t[1]['value']+t[3]['value']
                                elif(t[2] == '-'):
                                    t[0]['value'] = t[1]['value']-t[3]['value']
                                elif(t[2] == '*'):
                                    t[0]['value'] = t[1]['value']*t[3]['value']
                                elif(t[2] == '/'):
                                    t[0]['value'] = float(t[1]['value'])/t[3]['value']
                                    t[0]['type'] = 'REAL'
                            else:
                                print "ERROR : division by 0 exception" , " at line no ", t.lineno(2)
             
                                flag2 = False                   
                                t[0]['value'] = None

                        else:
                            t[0]['value'] = None;
                            if(myType3=='INTEGER'):
                                tempu = newTemp('REAL');
                                tempQuad = Quad('intToReal',temp3,None,tempu);
                                temp3 = tempu
                    if(myType3=='BOOLEAN' or myType3=='CHAR'):
                        print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
             
                
                        flag2 = False

            if(myType1=='BOOLEAN'):
                if(myType3=='BOOLEAN'):
                    print "ERROR : Invalid type for operator", " at line no ", t.lineno(2)
             
                    flag2 = False

                else:
                    print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
             
                    flag2 = False

            if(myType1=='CHAR'):
                print "ERROR : Incompatible type for operator" , " at line no ", t.lineno(2)
             
                flag2 = False    
        
        
        # Intermediate code generation
        newQuadList = []
        if(tempQuad is not None):
            #quadList.append(tempQuad)
            emit(tempQuad);
            newQuadList.append(tempQuad);
        var = t[0]['value'];
        
        #print "type!!! ",t[0]['type'] 
        result = newTemp(t[0]['type'])
        
        t[0]['place'] = result
        
        
        if(var is not None):
            quad = Quad(None, var , None, result)
        else:
            if(opType is not None):
                quad = Quad(t[2], temp1, temp3, result, opType)
            else:
                quad = Quad(t[2], temp1, temp3, result)    
            
        #quadList.append(quad)
        emit(quad);
        
        newQuadList = t[1]['code'][:].extend(t[3]['code'][:])
        if(newQuadList is None):
            newQuadList = []
        #print newQuadList
        
        newQuadList.append(quad)
        t[0]['code'] = newQuadList    

    if(debug4):
        print t[1] , t[3]        
        print "t[0] passed " , t[0];
            
def p_factor(t):    
    '''factor       :     NIL ''';
   
def p_factor_designator(t):
    '''factor : designator'''
    if(debug2):
        print "Designator"   ,t[1]
    toPass={};
    #toPass['trueList'] = [];
    #toPass['falseList'] = [];
    
    #to modify t[1]['place'] and t[1]['code']
    
    if(t[1]['what']!='PROCEDURE'):   #Variable or Array or Pointer
        toPass['type'] = t[1]['type'];
        toPass['value'] = None; 
        toPass['place'] = t[1]['place']
        toPass['code'] = t[1]['code']
        toPass['what'] = t[1]['what']
        
        if(toPass['type'] == 'BOOLEAN'):
            toPass['trueList'] = makeList(nextQuad())#t[1]['trueList'];
            toPass['falseList'] = makeList(nextQuad()+1)#t[1]['falseList'];
            quad = Quad('ifgoto', toPass['place'], None, None)
            emit(quad)
            quad = Quad('goto', None, None, None)
            emit(quad)
        
        #t[0] = (t[1]['type'], None);  #This is a dangerous fix      
    else:#if(len(t[1])==4): #Procedure
        toPass['type'] = t[1]['returnType'];
        toPass['value'] = None;
        toPass['place'] = t[1]['place']
        toPass['code'] = [];#t[1]['code']
        toPass['what'] = t[1]['what']
        #t[0]=(t[1]['returnType'], None)        
    t[0] = toPass;    
 
def p_factor_integer(t):
    '''factor : INTEGERCONSTANT'''
    #t[0] = ('INTEGER',t[1])
    toPass={};
    toPass['type'] = 'INTEGER'
    toPass['value'] = t[1]; 
    toPass['place'] = t[1]
    toPass['code'] = []
    toPass['what'] = 'INTEGERCONSTANT'
    
    t[0] = toPass;
       
def p_factor_character(t):
    '''factor : CHARACTERCONSTANT'''
    #t[0] = ('CHAR',t[1])
    toPass={};
    toPass['type'] = 'CHAR'
    toPass['value'] = t[1];
    toPass['place'] = t[1]
    #toPass['code'] = " "
    toPass['code'] = [] 
    toPass['what'] = 'CHARACTERCONSTANT'
    t[0] = toPass;
            
def p_factor_real(t):
    '''factor : REALCONSTANT'''
    #t[0] = ('REAL',t[1]);
    toPass={};
    toPass['type'] = 'REAL'
    toPass['value'] = t[1]; 
    
    
    toPass['place'] = t[1]
    #toPass['code'] = " "
    toPass['code'] = []
    toPass['what'] = 'REALCONSTANT'
    t[0] = toPass;    
    
def p_factor_string(t):
    '''factor : STRINGCONSTANT'''
    #t[0] = ('STRING',t[1]);
    toPass={};
    toPass['type'] = 'STRINGCONSTANT'
    toPass['value'] = t[1]; 
    
    toPass['place'] = t[1]
    #toPass['code'] = " "
    toPass['code'] = []
    toPass['what'] = 'STRINGCONSTANT'
    
    t[0] = toPass;    
    
def p_factor_boolean(t):
    '''factor : BOOLEANCONSTANT'''
    #t[0] = ('BOOLEAN',t[1]);
    global debug4
    toPass={};
    toPass['type'] = 'BOOLEAN'
    #toPass['code'] = " "
    toPass['code'] = []
    toPass['what'] = 'BOOLEANCONSTANT'
    if(t[1]=="TRUE"):
        toPass['value'] = True 
        toPass['place'] = "True"
    else:
        toPass['value'] = False 
        toPass['place'] = "False"
    if(toPass['value']):    
        toPass['trueList'] = makeList(nextQuad());
        toPass['falseList'] = [];
    else:
        toPass['trueList'] = [];
        toPass['falseList'] = makeList(nextQuad());    
    q = Quad('goto');
    emit(q);    
    t[0] = toPass
    if(debug4):
        print "printing hello", t[0];
     
            
def p_factor_neg(t):
    '''factor : '~' factor '''
    global flag2
    global debug4
    if(t[2]['type'] == 'BOOLEAN'):
        if(t[2]['value'] is not None):
            if(t[2]['value']):
                value = False
            else:
                value = True
            place = value
            code = []
        
        else:
            value = None
            # I wrote (Sailesh) the next two lines on 15th April
            place = t[2]['place']
            code = []
        '''
            temp = newTemp('BOOLEAN');
            quad = Quad(t[1], t[2]['place'], None, temp);
            place = temp; 
            
            #quadList.append(quad);
            emit(quad);
        
            newQuadList = []
            newQuadList = t[2]['code'][:]
            if(newQuadList is None):
                newQuadList = []
            #print newQuadList
            newQuadList.append(quad)
            code = newQuadList
        '''
    else:
        print "ERROR : In factor, only boolean allowed " , " at line no ", t.lineno(1)
        flag2 = False
    #else:
    # We are assuming that boolean occur    
    #t[0] = ('BOOLEAN',value);
    t[0] = {}
    t[0]['type'] = 'BOOLEAN'
    #toPass = {};
    #toPass['type'] = 'BOOLEAN';
    t[0]['value'] = value
    t[0]['place'] = place
    t[0]['code'] = code
    t[0]['what'] = 'NEG(Factor)'
    t[0]['trueList'] = t[2]['falseList'];
    t[0]['falseList'] = t[2]['trueList']; 
    if(debug4):
        print "truelist",t[0]['trueList']
        print "falselist", t[0]['falseList']
    
    #toPass['value'] = value;
    #toPass['place'] = str(value)
    #toPass['code'] = " "
    #t[0] = toPass;
        
def p_bracket_expr(t):
    '''factor : '(' expression ')' '''  
    #t[0] = t[2];
      
    toPass = {};
    toPass['type'] = t[2]['type'];
    toPass['value'] = t[2]['value'];
    toPass['place'] = t[2]['place']
    toPass['code'] = t[2]['code']
    toPass['what'] = '(expr)' 
     
    t[0] = toPass;
    if(t[2]['type'] == 'BOOLEAN'):
        t[0]['trueList'] = t[2]['trueList']
        t[0]['falseList'] = t[2]['falseList']
    
    
        
    
#In Case of unsuccessful parse, identify the source of error
def p_error(t):
    print "Unsuccessful parse at ",t
    global flag
    flag = False

#The following is the precedence order of the files
precedence = ( ('left', '=', '#'),
               ('left' , 'OR'),
               ('left', '&'), 
              ('left' , '<', 'LESSEQUAL', '>', 'MOREEQUAL'), 
              ('left','+','-'),
              ('left','*','/', 'DIV', 'MOD'),
              ('left' ,'UPLUS', 'UMINUS')
 )

import ply.yacc as yacc
yacc.yacc(start = 'module', debug = True)

#The input file is set here
if(len(sys.argv) > 1):
    f = open(sys.argv[1], 'r')
else:
    # The default file is 'input.txt'
    f = open('input.txt', 'r')    
s = f.read() # Read the input file into s
lexer.input(s) # The lexer reads from s


# Get tokens from the Lexer
if(tokenPrinting):
    while(True):
        tok = lexer.token()
        if not tok:
            break
        print tok

#Parse the file
yacc.parse(s, tracking=True)

if(debug2):
    print len(stack);
    stack[0].Print();
    #stack[1].Print()

if(debug3):
    print "\nQUADLIST\n"
    j = 0
    for i in quadList:
        #print j, " : "
        i.Print(j);
        print ""
        j+=1
        
def result():
    global flag, flag2
    
    if(flag):
        print "\nSuccessful parse : No syntax errors";
        if(flag2):
            print "Code is semantically correct ";
        else:
            print "Semantic errors were found";
    else:
        print "Unsuccessful parse"
    print "halt" ;
    #exit()

result()

# As soon as you see an error , change flag to false!
if(debug4):
    print "Target List", targetList;
targetList = list(set(targetList));
if(debug4):
    print "Target List", targetList;
