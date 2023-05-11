module Solucion where
-- Completar con los datos del grupo
--
-- Nombre de Grupo: Grupo.hs
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

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

-- Ejercicio 1

-- Dada una red social, devuelve una lista con los nombres de usuario (sin id) de cada usuario en la red
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (u:us,rs,ps) | longitud us == 0 = [nombreDeUsuario u]
                               | pertenece u us = nombresDeUsuarios (us,rs,ps)
                               | otherwise = nombreDeUsuario u : nombresDeUsuarios (us,rs,ps)

-- Devuelve la longitud de una lista
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = longitud xs + 1

-- Decide si un elemento pertenece a una lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

-- Ejercicio 2

-- Dada una red social y un usuario, devuelve una lista con sus amigos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = amigosDeAux (relaciones r) u

-- Recorre la lista de relaciones de la red social buscando aquellas relaciones en las que el usuario pasado como parametro aparezca en uno de los extremos
amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux (rs:rt) u | fst rs == u = snd rs:amigosDeAux rt u
                      | snd rs == u = fst rs:amigosDeAux rt u
                      | otherwise = amigosDeAux rt u

-- Ejercicio 3

-- Dado un usuario, devuelve la cantidad de amigos que tiene
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = fromIntegral (longitud (amigosDe rs us))

-- Ejercicio 4

-- Dada una red social, devuelve al usuario que tenga más amigos en la red
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (u:us, rs, ps) | longitud us == 0 = u
                                   | tieneMasAmigos (u:us,rs,ps) u = u
                                   | otherwise = usuarioConMasAmigos (us,rs,ps)

-- Compara la cantidad de amigos de un usuario con la cantidad de amigos de otro usuario
tieneMasAmigos :: RedSocial -> Usuario -> Bool
tieneMasAmigos (u:us,rs,ps) user | longitud us == 0 && cantidadDeAmigos red u <= cantidadDeAmigos red user = True
                                 | cantidadDeAmigos red u <= cantidadDeAmigos red user = tieneMasAmigos (us, rs, ps) user
                                 | otherwise = False
                                 where red = (u:us,rs,ps)

-- Ejercicio 5

-- Dada una red social, busca si existe un usuario que tenga más de 1 millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos r = cantidadDeAmigos r (usuarioConMasAmigos r) > 1000000

-- Ejercicio 6

-- Dada una red social y un usuario, devuelve las publicaciones que subió
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,p:ps) user | longitud ps == 0 && usuarioDePublicacion p == user = [p]
                                  | longitud ps == 0 && usuarioDePublicacion p /= user = []
                                  | usuarioDePublicacion p == user = p : publicacionesDe (us,rs,ps) user
                                  | otherwise = publicacionesDe (us,rs,ps) user

-- Ejercicio 7

-- Dada una red social y un usuario, devuelve las publicaciones a las que le dio like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAAux (publicaciones r) u

-- Recorre la lista de publicaciones y para cada una verifica si el usuario pasado como argumento está en la lista de likes de la publicación
publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] u = []
publicacionesQueLeGustanAAux (ps:pt) u | publicacionLikeada (likesDePublicacion ps) u = ps : publicacionesQueLeGustanAAux pt u
                                       | otherwise = publicacionesQueLeGustanAAux pt u

-- Recibe una lista de usuarios y un usuario en particular y verifica que ese usuario aparezca en la lista
publicacionLikeada :: [Usuario] -> Usuario -> Bool
publicacionLikeada [] u = False
publicacionLikeada (x:xs) u | x == u = True
                            | otherwise = publicacionLikeada xs u

-- Ejercicio 8

-- Dada una red social y dos usuarios, verifica si les gustan las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = publicacionesQueLeGustanA r u1 == publicacionesQueLeGustanA r u2

-- Ejercicio 9

-- Dada una red social y un usuario verifica si existe un usuario que le haya dado like a todas las publicaciones de otro usuario, que tienen que ser al menos una
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u | longitud (publicacionesDe r u) == 0 = False
                        | otherwise = tieneUnSeguidorFielAux2 (tail (publicacionesDe r u)) (likesDePublicacion (head (publicacionesDe r u)))

-- Verifica si alguno de los usuarios de la lista de likes de la primera publicación es un seguidor fiel
tieneUnSeguidorFielAux2 :: [Publicacion] -> [Usuario] -> Bool
tieneUnSeguidorFielAux2 _ [] = False
tieneUnSeguidorFielAux2 [] us = longitud us >= 1
tieneUnSeguidorFielAux2 p (u:us) | longitud us == 0 = tieneUnSeguidorFielAux p u
                                 | otherwise = tieneUnSeguidorFielAux p u || tieneUnSeguidorFielAux2 p us

-- Verifica si un usuario ha dado like a todas las publicaciones de 'u'
tieneUnSeguidorFielAux :: [Publicacion] -> Usuario -> Bool
tieneUnSeguidorFielAux (p:ps) u | longitud ps == 0 = pertenece u (likesDePublicacion p)
                                | pertenece u (likesDePublicacion p) = tieneUnSeguidorFielAux ps u
                                | otherwise = False

-- Ejercicio 10

-- Dada una red social y dos usuarios verifica si existe una secuencia de usuarios amigos que conecta 'u1' y 'u2'
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 =  buscoPor r u1 (amigosDe r u2) [u2] || buscoPor r u2 (amigosDe r u1) [u1]

-- Busca si 'u' está en la lista de amigos. Si 'u' no está en la lista de amigos, llama a la función buscoPorCadaAmigo
buscoPor :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
buscoPor r u amigos vistos | listaASinB amigos vistos == [] = False
                           | pertenece u amigos = True
                           | otherwise = buscoPorCadaAmigo r u amigos vistos

-- Busca si alguno de los amigos es 'u' o tiene un camino hacia 'u'
buscoPorCadaAmigo :: RedSocial ->  Usuario -> [Usuario] -> [Usuario] -> Bool
buscoPorCadaAmigo r u [] vistos = False
buscoPorCadaAmigo r u (as:at) vistos = buscoPor r u (amigosDe r as) (vistos ++ [as]) || buscoPorCadaAmigo r u at (vistos ++ [as])

-- Dadas dos listas, devuelve una lista que contiene los elementos de la primera lista que no estan en la segunda lista
listaASinB :: [Usuario] -> [Usuario] -> [Usuario]
listaASinB [] _ = []
listaASinB _ [] = []
listaASinB (as:at) b | pertenece as b = listaASinB at b
                     | otherwise = as : listaASinB at b