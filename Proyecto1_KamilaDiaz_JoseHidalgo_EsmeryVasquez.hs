{- Proyecto 1 Programación Funcional: Evaluador de Operaciones -}

-- TIPOS ALGEBRAICOS
-- tipo 'Operador', para separar la estructura de operación
data Operador = Suma | Resta | Mult | Division
-- tipo 'Expresion' que representa el árbol de una expresión aritmética, tipo recursivo
data Expresion = Numero Int | Aplicar Operador Expresion Expresion