import Data.Char

type NumeroTelefonico = String
type Saldo = Float
type Habilitado = Bool
data Cliente = UnCliente NumeroTelefonico Saldo Habilitado deriving (Show, Eq)

alf :: Cliente
alf = UnCliente "66660000" 5 True
debi :: Cliente
debi = UnCliente "77770000" (-20) True

numeroTelefonico :: Cliente -> NumeroTelefonico
numeroTelefonico (UnCliente numero _ _) = numero

saldo :: Cliente -> Saldo
saldo (UnCliente _ saldo _) = saldo

habilitado :: Cliente -> Habilitado
habilitado (UnCliente _ _ habil) = habil

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
disminuirSaldo (UnCliente numero saldo habil) saldoADisminuir =
    UnCliente numero (saldo - saldoADisminuir) habil

aumentarSaldo :: Cliente -> Saldo -> Cliente
aumentarSaldo (UnCliente numero saldo habil) saldoAAumentar =
    UnCliente numero (saldo + saldoAAumentar) habil

bloquear :: Cliente -> Cliente
bloquear (UnCliente numero saldo _) = UnCliente numero saldo False



cargarSaldo :: Cliente -> Saldo -> Dia -> Cliente
cargarSaldo cliente monto dia
    | esFinde dia = aumentarSaldo cliente (2 * monto) 
    | otherwise = aumentarSaldo cliente monto



