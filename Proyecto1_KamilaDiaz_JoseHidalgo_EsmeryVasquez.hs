{- Proyecto 1 Programacion Funcional: Evaluador de Operaciones -}

{- ============TIPOS ALGEBRAICOS============ -}
-- tipo 'Operador', para separar la estructura de operacion
data Operador = Suma | Resta | Mult | Division deriving Read

-- tipo 'Expresion' que representa el arbol de una expresion aritmética, tipo recursivo
data Expresion = Numero Int | Aplicar Operador Expresion Expresion deriving Read

{- ================FUNCIONES================ -}
{-
Aplicar la operacion
Aplica un operador a dos enteros
Devuelve Maybe para manejar errores, Just resultado si la operacion es valida y Nothing si ocurre un error
-}
aplicarOperacion :: Operador -> Int -> Int -> Maybe Int
aplicarOperacion Suma x y = Just (x + y)
aplicarOperacion Resta x y = Just (x - y)
aplicarOperacion Mult x y = Just (x * y)
aplicarOperacion Division _ 0 = Nothing
aplicarOperacion Division x y = Just (x `div` y)

{-
Combina dos valores de tipo Maybe Int
Si alguno de los argumentos es Nothing, devuelve Nothing. Si ambos son Just aplica la funcion que recibe
Si hay error en alguna subexpresion, el error se propaga de manera automatica.
-}
combinar :: Maybe Int -> Maybe Int -> (Int -> Int -> Maybe Int) -> Maybe Int
combinar Nothing _ _ = Nothing
combinar _ Nothing _ = Nothing
combinar (Just x) (Just y) f = f x y

{-
Funcion 'evaluar' recursiva que procesa una expresion representada como un arbol.
Si es un Numero, devuelve Just n.
Si es una Aplicacion de operador, evalua ambas subexpresiones y aplica el operador si ambas son validas.
Si ocurre un error (por ejemplo division por cero),devuelve Nothing.
-}
evaluar :: Expresion -> Maybe Int
evaluar (Numero n) = Just n
evaluar (Aplicar op e1 e2) =
    combinar (evaluar e1)
            (evaluar e2)
            (aplicarOperacion op)

{- ====================MAIN==================== -}
-- Main con ejemplo de expresion valida e invalida
main :: IO ()
main = do
    let expresion = 
            Aplicar Mult 
                (Aplicar Suma (Numero 8) (Numero 4))
                (Aplicar Resta (Numero 3) (Numero 1))
        expresionError = Aplicar Division (Numero 5) (Numero 0)

    putStrLn "Expresion valida '(8 + 4) * (3 - 1)': "
    print(evaluar expresion)
    putStrLn "Expresion invalida '5/0': "
    print(evaluar expresionError)
    putStrLn "Introduzca una expresion: "
    exp1 <- readLn :: IO Expresion
    print(evaluar exp1)