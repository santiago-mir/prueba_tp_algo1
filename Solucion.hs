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

-- describir qué hace la función: Devuelve una lista con los nombres de usuario (sin id) de cada usuario en la red
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = longitud xs + 1

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) | e == x = True
                   | otherwise = pertenece e xs

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (u:us,rs,ps) | longitud us == 0 = [nombreDeUsuario u]
                               | pertenece u us = nombresDeUsuarios (us,rs,ps)
                               | otherwise = nombreDeUsuario u : nombresDeUsuarios (us,rs,ps)

-- describir qué hace la función: Dado un usuario, devuelve una lista con sus amigos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = amigosDeAux (relaciones r) u

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux (rs:rt) u | fst rs == u = snd rs:amigosDeAux rt u
                      | snd rs == u = fst rs:amigosDeAux rt u
                      | otherwise = amigosDeAux rt u

-- describir qué hace la función: Dado un usuario, devuelve la cantidad de amigos que tiene
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos rs us = fromIntegral (longitud (amigosDe rs us))

-- describir qué hace la función: Devuelve al usuario que tenga más amigos en la red
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (u:us, rs, ps) | longitud us == 0 = u
                                   | tieneMasAmigos (u:us,rs,ps) u = u
                                   | otherwise = usuarioConMasAmigos (us,rs,ps)

tieneMasAmigos :: RedSocial -> Usuario -> Bool
tieneMasAmigos (u:us,rs,ps) user | longitud us == 0 && cantidadDeAmigos red u <= cantidadDeAmigos red user = True
                                 | cantidadDeAmigos red u <= cantidadDeAmigos red user = tieneMasAmigos (us, rs, ps) user
                                 | otherwise = False
                                 where red = (u:us,rs,ps)

-- describir qué hace la función: Existe un usuario que tenga más de 1 millón de amigos?
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos r = cantidadDeAmigos r (usuarioConMasAmigos r) > 1000000

-- describir qué hace la función: Dado un usuario, devuelve las publicaciones que subió
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,p:ps) user | longitud ps == 0 && usuarioDePublicacion p == user = [p]
                                  | longitud ps == 0 && usuarioDePublicacion p /= user = []
                                  | usuarioDePublicacion p == user = p : publicacionesDe (us,rs,ps) user
                                  | otherwise = publicacionesDe (us,rs,ps) user

-- describir qué hace la función: Dado un usuario, devuelve las publicaciones a las que le dio like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAAux (publicaciones r) u

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] u = []
publicacionesQueLeGustanAAux (ps:pt) u | publicacionLikeada (likesDePublicacion ps) u = ps : publicacionesQueLeGustanAAux pt u
                                       | otherwise = publicacionesQueLeGustanAAux pt u

publicacionLikeada :: [Usuario] -> Usuario -> Bool
publicacionLikeada [] u = False
publicacionLikeada (x:xs) u | x == u = True
                            | otherwise = publicacionLikeada xs u

-- describir qué hace la función: Dados dos usuarios, les gustan las mismas publicaciones?
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = publicacionesQueLeGustanA r u1 == publicacionesQueLeGustanA r u2

-- describir qué hace la función: Existe un usuario que le haya dado like a todas las publicaciones de otro usuario, que tienen que ser al menos una
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u | longitud (publicacionesDe r u) == 0 = False
                        | otherwise = tieneUnSeguidorFielAux2 (tail (publicacionesDe r u)) (likesDePublicacion (head (publicacionesDe r u)))

tieneUnSeguidorFielAux2 :: [Publicacion] -> [Usuario] -> Bool
tieneUnSeguidorFielAux2 _ [] = False
tieneUnSeguidorFielAux2 [] us = longitud us >= 1
tieneUnSeguidorFielAux2 p (u:us) | longitud us == 0 = tieneUnSeguidorFielAux p u
                                 | otherwise = tieneUnSeguidorFielAux p u || tieneUnSeguidorFielAux2 p us

tieneUnSeguidorFielAux :: [Publicacion] -> Usuario -> Bool
tieneUnSeguidorFielAux (p:ps) u | longitud ps == 0 = pertenece u (likesDePublicacion p)
                                | pertenece u (likesDePublicacion p) = tieneUnSeguidorFielAux ps u
                                | otherwise = False

-- describir qué hace la función: Dada una secuencia de usuarios de la red, mayor o igual a 2, que empiece con el usuario 1 y termine con el usuario 2, hay una cadena de amigos de esa secuencia
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 =  buscoPor r u1 (amigosDe r u2) [u2] || buscoPor r u2 (amigosDe r u1) [u1]

buscoPor :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
buscoPor r u amigos vistos | listaASinB amigos vistos == [] = False
                           | pertenece u amigos = True
                           | otherwise = buscoPorCadaAmigo r u amigos vistos

buscoPorCadaAmigo :: RedSocial ->  Usuario -> [Usuario] -> [Usuario] -> Bool
buscoPorCadaAmigo r u [] vistos = False
buscoPorCadaAmigo r u (as:at) vistos = buscoPor r u (amigosDe r as) (vistos ++ [as]) || buscoPorCadaAmigo r u at (vistos++[as])

listaASinB :: [Usuario] -> [Usuario] -> [Usuario]
listaASinB [] _ = []
listaASinB _ [] = []
listaASinB (as:at) b | pertenece as b = listaASinB at b
                     | otherwise = as : listaASinB at b