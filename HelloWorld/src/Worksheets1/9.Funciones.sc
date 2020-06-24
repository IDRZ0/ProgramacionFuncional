def saludo(nombre: String) = print("Hola " + nombre)
saludo("Melo")

def saludosVarios(idioma: String) =
  if (idioma == "ES")
    (nombre: String) => print("Hola " + nombre)
  else if (idioma == "FR")
    (nombre: String) => print("Bonjour " + nombre)
  else
    (nombre: String) => print("Hello " + nombre)

def saludos2(idioma: String) = (nombre: String) =>
  (if (idioma == "ES")
    "Hola "
  else if (idioma == "FR")
    "Bonjour "
  else
    "Hello ") + nombre

def saludoES = saludosVarios("ES")
saludoES("Melito")

def saludoES2 = saludos2("ES")
saludoES2("Melo")

saludos2("EN")("Melon")