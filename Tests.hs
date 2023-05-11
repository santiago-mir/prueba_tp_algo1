module Tests where

import Test.HUnit
import SolucionM as Implementacion
import SolucionS 
import SolucionL 

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (Implementacion.nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (Implementacion.amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (Implementacion.cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (Implementacion.usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (Implementacion.estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (Implementacion.publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (Implementacion.publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (Implementacion.lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (Implementacion.tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (Implementacion.existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)


usuario6 = (6, "José")
usuario7 = (7, "Manuel")
usuario8 = (8, "Esteban")
usuario9 = (9, "Juana")

relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)

relacion2_6 = (usuario2, usuario6)
relacion2_7 = (usuario2, usuario7)
relacion2_8 = (usuario2, usuario8)
relacion2_9 = (usuario2, usuario9)

relacion3_6 = (usuario3, usuario6)
relacion3_7 = (usuario3, usuario7)
relacion3_8 = (usuario3, usuario8)
relacion3_9 = (usuario3, usuario9)

relacion4_6 = (usuario4, usuario6)
relacion4_7 = (usuario4, usuario7)
relacion4_8 = (usuario4, usuario8)
relacion4_9 = (usuario4, usuario9)

relacion5_6 = (usuario5, usuario6)
relacion5_7 = (usuario5, usuario7)
relacion5_8 = (usuario5, usuario8)
relacion5_9 = (usuario5, usuario9)