#Codigo - Proyecto (LEX Y YACC)

#JOVEN JIMENEZ ANGEL CRISTIAN
#MARTINEZ RIOS LEONARDO DANIEL

#22 Mayo 2023 - Compiladores | 2657

#Importamos las librerias:
import ply.lex as lex #Proporciona las funcionalidades para implementar el analizador lexico.
import ply.yacc as yacc #Proporciona las funcionalidades para implementar el analizador sintactico.

#Definición de tokens lexicos.
#En este caso, vamos a evaluar expresiones aritmeticas basicas, 
#para ello, vamos a declarar tokens de las operaciones basicas.
tokens = (
    'Numero', #Token para representar numeros.
    'Suma', #Token para representar operador de suma.
    'Resta', #Token para representar operador de resta.
    'Multiplicacion', #Token para representar operador de multiplicacion.
    'Division', #Token para representar operador de division.
    'parentesis_Izquierdo', #Token para representar el caracter de parentesis izquierdo.
    'parentesis_Derecho', #Token para representar el caracter de parentesis derecho.
)

#Reglas de expresiones regulares para los tokens.
#Vamos a generar nuestras reglas o expresiones respecto a nuestros tokens generados previamente.
t_Numero = r'\d+' #Expresion regular que coincide con uno o mas dígitos.
t_Suma = r'\+' #Expresion regular para el operador '+'
t_Resta = r'-' #Expresion regular para el operador '-'
t_Multiplicacion = r'\*' #Expresion regular para el operador '*'
t_Division = r'/' #Expresion regular para el operador '/'
t_parentesis_Izquierdo = r'\(' #Expresion regular para el caracter especial '('
t_parentesis_Derecho = r'\)' #Expresion regular para el caracter especial ')'
t_ignore = ' \t' #Expresion regular para ignorar los caracteres de espacio en blanco

#Esta funcion servira para analizar la expresion y buscar caracteres ilegales (si las hubiese).
#Si detecta un caracter ilegal (como es una letra, la cual no esta definida en nuestros tokens),
#va a mostrarle al usuario que caracter (o caracteres) ilegal ingreso, despues se va a omitir
#dicho caracter y seguira buscando en nuestra expresion mas caracteres ilegales.
def t_error(t):
    print("Caracter ilegal encontrado: '%s'" % t.value[0])
    t.lexer.skip(1)

#Construimos nuestro analizador léxico.
analizador_Lexico = lex.lex()

#Reglas de precedencia de operadores.
#Aqui se especifica el orden de evalaur los operadores de una expresion matematica. 
#La precedencia determina que operadores se evaluan primero y como es que se agrupan los operandos.
precedence = (
    ('left', 'Suma', 'Resta'),
    ('left', 'Multiplicacion', 'Division'),
)

#Reglas de produccion sintactica.
#Esta funcion define todas las reglas de produccion que hara nuestro analisis sintactico.
#En este caso, nuestro programa va a evaluar expresiones matematicas, 
#para ello, creamos 4 reglas de produccion, cada una representando una operación aritmética diferente.
def p_reglas_Expresiones(almacena_Expresion):
    '''
    expresion : expresion Suma expresion
              | expresion Resta expresion
              | expresion Multiplicacion expresion
              | expresion Division expresion
    '''
    #Aqui hacemos validaciones dependiendo de la operacion aritmetica a realizar, donde:
    #almacena_Expresion[0]: Almacena el resultado de la operacion.
    #almacena_Expresion[1]: Almacena el valor de la primera expresion antes del operador.
    #almacena_Expresion[2]: Almacena el valor de algun operador.
    #almacena_Expresion[3]: Almacena el valor de la segunda expresión después del operador.

    if almacena_Expresion[2] == '+':
        almacena_Expresion[0] = almacena_Expresion[1] + almacena_Expresion[3]
    elif almacena_Expresion[2] == '-':
        almacena_Expresion[0] = almacena_Expresion[1] - almacena_Expresion[3]
    elif almacena_Expresion[2] == '*':
        almacena_Expresion[0] = almacena_Expresion[1] * almacena_Expresion[3]
    elif almacena_Expresion[2] == '/':
        #Si el valor de la segunda expresion despues del operador es diferente de 0, ejecuta la division.
        #Si no, entonces imprime al usuario un error de que no se puede dividir entre 0.
        if almacena_Expresion[3] != 0:
            almacena_Expresion[0] = almacena_Expresion[1] / almacena_Expresion[3]
        else:
            print("Error: No se puede dividir entre cero")
            almacena_Expresion[0] = 0

#Se crea una regla de produccion especifica.
#Esta regla va a indicar que una expresion consiste solo en un numero. 
#Si esto es valido, entonces ejecuta dicha funcion.
#Si no, entonces retorna al usuario un error.
def p_expresion_Numero(almacena_Expresion):
    'expresion : Numero'
    almacena_Expresion[0] = int(almacena_Expresion[1])

#Se crea una regla de produccion especifica.
#Esta regla va a indicar que una expresion puede contener parentesis.
#Sin embargo, para que esto sea valido, la expresion debe de contener un paréntesis izquierdo, una expresión y un paréntesis derecho.
#Si esto es valido, entonces ejecuta dicha funcion.
#Si no, entonces retorna al usuario un error.
def p_expresion_Parentesis(almacena_Expresion):
    'expresion : parentesis_Izquierdo expresion parentesis_Derecho'
    almacena_Expresion[0] = almacena_Expresion[2]

#Se crea una funcion la cual servira (en este caso)
#detectar errores de sintaxis en nuestro analisis sintactico, 
#mas en concreto, errores que se presentan en nuestras reglas de produccion.
#Si detecta un error, entonces se le imprime al usuario "Error de sintaxis".
def p_error(almacena_Expresion):
    print("Error de sintaxis")

#Construimos nuestro analizador sintáctico.
analizador_Sintactico = yacc.yacc()

#Esta funcion servira para poder realizar el analisis 
#de la expresion ingresada por el usuario.
def analizar_Expresion(ingresar_Expresion):
    resultado_Expresion = analizador_Sintactico.parse(ingresar_Expresion)
    return resultado_Expresion

#Definimos nuestra funcion principal, la cual va a contener un menu principal
#en el cual el usuario podra seleccionar 2 opciones:
#1. Ingresar una expresion y 2. Salir del programa
#Si el usuario selecciona la opcion 1, entonces le va a pedir al usuario 
#que ingrese una expresion, despues va a analizar dicha expresion y va a retornar
#el resultado del analisis, asi como el resultado de la operacion aritmetica.
#Asimismo, le va a preguntar de nuevo al usuario que opcion va a elegir, asi para continuar con el bucle.
#Si ahora la opcion es = 2, entonces el programa va a terminar.
#Si la opcion que ingrese el usuario es diferente a 1 o 2, 
#entonces le va a pedir al usuario que ingrese una opcion valida.
def menu_Principal():
    print("¡BIENVENIDO!\n¿Que desea realizar?: ")
    print("1. Ingresar una expresion\n2. Salir del programa\n")
    while True:
        elegir_Opcion = int(input("Eliga una opcion: "))
        if elegir_Opcion == 1:
            ingresar_Expresion = input("\nIngrese la expresion: ")
            resultadoOperacion_Expresion = analizar_Expresion(ingresar_Expresion)
            print(f"Resultado de la expresión '{ingresar_Expresion}' = {resultadoOperacion_Expresion}")
            print("\n¿Que es ahora lo que desea realizar?: ")
            print("1. Ingresar una expresion\n2. Salir del programa\n")
        elif elegir_Opcion == 2:
            print("\n¡Hasta Luego!\n")
            return 0
        else:
            print("\n¡Opcion no valida!\nEliga de nuevo alguna opcion valida!")

#Ejecuta la funcion principal.
menu_Principal()