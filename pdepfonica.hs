type Cliente = String

alf :: Cliente
alf = "66660000"
debi :: Cliente
debi = "77770000"

esNuevo :: Cliente -> Bool
esNuevo = (== '7') . head 