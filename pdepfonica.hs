import Data.Char

type NumeroTelefonico = String
type Saldo = Float
type Bloqueado = Bool
type Cliente = (NumeroTelefonico, Saldo, Bloqueado)

alf :: Cliente
alf = ("66660000", 5)
debi :: Cliente
debi = ("77770000", -20)

numeroTelefonico :: Cliente -> NumeroTelefonico
-- numeroTelefonico (numero, _) = numero
numeroTelefonico = fst

saldo :: Cliente -> Saldo
-- saldo (_, saldo) = saldo
saldo = snd

esNuevo :: Cliente -> Bool
esNuevo = (== '7') . head . numeroTelefonico

esMoroso :: Cliente -> Bool
esMoroso = esNegativo . saldo

esNegativo = (<0)

data Dia = 
    Lunes | Martes 
    | Miercoles | Jueves 
    | Viernes | Sabado | Domingo
    deriving (Show, Eq, Ord)

esFinde :: Dia -> Bool
esFinde Sabado = True
esFinde Domingo = True
esFinde _ = False


disminuirSaldo :: Cliente -> Saldo -> Cliente
-- disminuirSaldo cliente saldoADisminuir =
--     (numeroTelefonico cliente, saldo cliente - saldoADisminuir)
disminuirSaldo (numero, saldo) saldoADisminuir =
    (numero, saldo - saldoADisminuir)

aumentarSaldo :: Cliente -> Saldo -> Cliente
aumentarSaldo (numero, saldo) saldoAAumentar =
    (numero, saldo + saldoAAumentar)


cargarSaldo :: Cliente -> Saldo -> Dia -> Cliente
cargarSaldo cliente monto dia
    | esFinde dia = aumentarSaldo cliente (2 * monto) 
    | otherwise = aumentarSaldo cliente monto



