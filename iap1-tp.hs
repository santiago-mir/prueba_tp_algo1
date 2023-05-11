-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

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
relacion4_5 = (usuario4, usuario5)




publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_1i = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2, publicacion2_1i]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3, relacion4_5 ]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)



-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios


longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys


-- Recorre la lista de usuarios de la red social y toma los nombres de cada usuario, sin tener en cuenta usuarios repetidos

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ((u:us), rs, ps) | longitud us == 0 = [nombreDeUsuario u]
                                   | pertenece u us   = nombresDeUsuarios (us, rs, ps)
                                   | otherwise        = nombreDeUsuario u : nombresDeUsuarios (us, rs, ps)


-- describir qué hace la función: .....

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (u, (r:rs), ps) user | longitud rs == 0 && not (estaEnUnaRelacion r user) = []
                              | longitud rs == 0 && estaEnUnaRelacion r user       = [userRelacionado r user]
                              | estaEnUnaRelacion r user                           = userRelacionado r user : amigosDe (u, rs, ps) user
                              | otherwise                                          = amigosDe (u, rs, ps) user

estaEnUnaRelacion :: Relacion -> Usuario -> Bool
estaEnUnaRelacion (u1, u2) user = u1 == user || u2 == user

userRelacionado :: Relacion -> Usuario -> Usuario
userRelacionado (u1, u2) user | u1 == user = u2
                              | u2 == user = u1

esLaMismaRelacion :: Relacion -> Relacion -> Bool
esLaMismaRelacion r1 r2 | (fst r1 == fst r2 && snd r1 == snd r2) || (fst r1 == snd r2 && snd r1 == fst r2) = True
                        | otherwise                                                                        = False

laRelacionPertenece :: Relacion -> [Relacion] -> Bool
laRelacionPertenece rel (r:rs) | longitud rs == 0 && not (esLaMismaRelacion r rel) = False
                               | esLaMismaRelacion r rel = True
                               | not (esLaMismaRelacion r rel) = laRelacionPertenece rel rs                       
-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = fromIntegral (longitud (amigosDe rs us))

 
-- describir qué hace la función: .....


tieneMasAmigos :: RedSocial -> Usuario -> Bool
tieneMasAmigos ((u:us), rs, ps) user | longitud us == 0 && cantidadDeAmigos red u <= cantidadDeAmigos red user = True
                                     | cantidadDeAmigos red u <= cantidadDeAmigos red user = tieneMasAmigos (us, rs, ps) user
                                     | otherwise = False
                                      where red = ((u:us), rs, ps) 


usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ((u:us), rs, ps) | longitud us == 0 = u
                                     | tieneMasAmigos ((u:us), rs, ps) u = u
                                     | otherwise                         = usuarioConMasAmigos (us, rs, ps)
                                     
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ((u:us), rs, ps) | longitud us == 0 && cantidadDeAmigos red u < 1000000 = False
                                   | cantidadDeAmigos red u > 1000000 = True
                                   | cantidadDeAmigos red u < 1000000 = estaRobertoCarlos (us, rs, ps) 
                                    where red = ((u:us), rs, ps) 
                                   


-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us, rs, (p:ps)) user | longitud ps == 0 && usuarioDePublicacion p == user = [p]
                                      | longitud ps == 0 && not (usuarioDePublicacion p == user) = []
                                      | usuarioDePublicacion p == user = p : publicacionesDe (us, rs, ps) user
                                      | otherwise = publicacionesDe (us, rs, ps) user

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us, rs, (p:ps)) user | longitud ps == 0 && not (pertenece user (likesDePublicacion p)) = []  
                                                | longitud ps == 0 && pertenece user (likesDePublicacion p) = [p]
                                                | pertenece user (likesDePublicacion p) = p : publicacionesQueLeGustanA (us, rs, ps) user
                                                | otherwise                             = publicacionesQueLeGustanA (us, rs, ps) user  

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs u1 u2 = publicacionesQueLeGustanA rs u1 == publicacionesQueLeGustanA rs u2

-- describir qué hace la función: .....

likeoTodas :: [Publicacion] -> Usuario -> Bool
likeoTodas [p] user    = pertenece user (likesDePublicacion p)  
likeoTodas (p:ps) user = pertenece user (likesDePublicacion p) && likeoTodas ps user

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel ((u:us), rs, ps) user | longitud us == 0 && likeoTodas (publicacionesDe red user) u = True
                                          | longitud us == 0 && not (likeoTodas (publicacionesDe red user) u) = False
                                          | likeoTodas (publicacionesDe red user) u = True
                                          | otherwise = tieneUnSeguidorFiel (us, rs, ps) user 
                                          where red = ((u:us), rs, ps)




-- describir qué hace la función: .....

secuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> [Relacion]
secuenciaDeAmigos ((u:us), rs, ps) u1 u2 | longitud us == 0 && pertenece u (amigosDe red u1) = [(u1, u)]
                                         | longitud us == 0 && not (pertenece u (amigosDe red u1)) = []
                                         | pertenece u (amigosDe red u1) = (u1, u) : secuenciaDeAmigos (us, rs, ps) u u2 
                                         | not (pertenece u (amigosDe red u1)) = secuenciaDeAmigos (us, rs, ps) u1 u2
                                         where red = ((u:us), rs, ps)
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos ((u:us), rs, ps) u1 u2 | pertenece u (amigosDe red u1) && pertenece u (amigosDe red u2) = True
                                               | pertenece u (amigosDe red u1) = False
                                               where red = ((u:us), rs, ps)
