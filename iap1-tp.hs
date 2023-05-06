-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU



---- TEST TEST TEST ----
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela") -- A
usuario5 = (5, "Natalia") -- B

relacion1_2 = (usuario1, usuario2) -- A y B
relacion1_3 = (usuario1, usuario3) -- /
relacion1_4 = (usuario4, usuario1) -- A      Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2) -- A y B
relacion2_4 = (usuario2, usuario4) -- A 
relacion3_4 = (usuario4, usuario3) -- A

relacion1_5 = (usuario1, usuario5) -- /
relacion2_5 = (usuario2, usuario5) -- /
relacion3_5 = (usuario3, usuario5) -- /
relacion4_5 = (usuario4, usuario5) -- /


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
redD = (usuariosB, relacionesB, [publicacion1_3, publicacion3_3,publicacion4_3]) -- Para probar lesGustanLasMismasPublicaciones
redE = (usuariosA, [relacion1_2, relacion2_3], publicacionesA) -- Para probar existeSecuenciaDeAmigos
--- RedC, para Roberto Carlos
crearUsuariosC n
    | n == 0 = [(n, show n)]
    | otherwise = (n, show n) : crearUsuariosC (n-1)
usuariosC = crearUsuariosC 1000010
crearRelacionesC n
    | n == 0 = robertoCarlos 1000010
    | otherwise = ((n, show n), (n-1, show (n-1))) : crearRelacionesC (n-1)
    where   robertoCarlos :: Integer -> [(Usuario, Usuario)]
            robertoCarlos n
                | n == 2 = [((1, "1"), (2,"2"))]
                | otherwise = ((1,"1"), (n, show n)) : robertoCarlos (n-1)
relacionesC =crearRelacionesC 1000010
publicacionesC = [((1,"1"), "Hola", [])]
redC = (usuariosC, relacionesC, publicacionesC)
---- TEST TEST TEST ----

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

----------
-- Funciones auxiliares genéricas
-- Dada una lista, devuelve cuántos elementos tiene
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


-- Dado un elemento y una lista, dice si el elemento pertenece a la lista
pertenece :: (Eq t) =>  t -> [t] -> Bool
pertenece _ [] = False
pertenece y (x:xs)
    | length xs == 0 = y == x
    | otherwise = y == x || pertenece y xs

-- Dada una lista, devuelve la misma en orden invertido
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Dada una lista, devuelve la misma lista dejando un único elemento por cada elemento distinto que tiene
quitarRepetidos :: (Eq t) => [t] -> [t]
quitarRepetidos [] = []
quitarRepetidos (x:xs)
    | longitud xs == 0 = [x]
    | pertenece x xs = x : quitarRepetidos (quitarTodos x xs) 
    | otherwise = x : quitarRepetidos xs
    where   quitarTodos :: (Eq t ) => t -> [t] -> [t] -- Quitar todos los t de la lista [t], requiere que t esté en la lista
            quitarTodos x xs
                | xs == [] || not (pertenece x xs) = xs
                | otherwise = quitarTodos x (quitar x xs)
            quitar :: (Eq t) => t -> [t] -> [t] -- Quitar el primer t de la lista [t], requiere que t esté en la lista
            quitar n (x:xs)
                | n == x = xs
                | otherwise = x : quitar n xs

----------
-- Ejercicios

-- describir qué hace la función: ..... Devuelve una lista con los nombres de usuario (sin id) de cada usuario en la red
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = reverso (quitarRepetidos (nombresDeUsuariosAux (usuarios red) []))
    where   nombresDeUsuariosAux :: [Usuario] -> [String] -> [String]
            nombresDeUsuariosAux (user:users) nombres
                | length users == 0 = nombre : nombres
                | otherwise = nombresDeUsuariosAux users (nombre : nombres)
                where   nombre = nombreDeUsuario user


-- describir qué hace la función: ..... Dado un usuario, devuelve una lista con sus amigos
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) user = quitarRepetidos(amigosDeAux rs)
    where   amigosDeAux :: [Relacion] -> [Usuario]
            amigosDeAux [] = []
            amigosDeAux (r:rel)
                | user1 == user = user2 : amigosDeAux rel
                | user2 == user = user1 : amigosDeAux rel
                | otherwise = amigosDeAux rel
                where   user1 = fst r
                        user2 = snd r


-- describir qué hace la función: ..... Dado un usuario, devuelve la cantidad de amigos que tiene
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red user = fromInteger(longitud (amigosDe red user))

-- describir qué hace la función: ..... Devuelve al usuario que tenga más amigos en la red
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red 
    where   usuarioConMasAmigosAux :: RedSocial -> Usuario
            usuarioConMasAmigosAux (user1:user2:us,rs,ps)
                | longitud us == 0 && cantidadDeAmigos red user1 >= cantidadDeAmigos red user2 = user1
                | longitud us == 0 && cantidadDeAmigos red user2 > cantidadDeAmigos red user1 = user2
                | cantidadDeAmigos red user1 >= cantidadDeAmigos red user2 = usuarioConMasAmigosAux (user1:us,rs,ps) 
                | otherwise = usuarioConMasAmigosAux (user2:us,rs,ps)

-- describir qué hace la función: ..... Existe un usuario que tenga más de 1 millón de amigos?
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 1000000

-- describir qué hace la función: ..... Dado un usuario, devuelve las publicaciones que subió
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (us,rs,ps) user = quitarRepetidos(publicacionesDeAux ps)
    where   publicacionesDeAux :: [Publicacion] -> [Publicacion]
            publicacionesDeAux [] = []
            publicacionesDeAux (p:pub)
                | publicador == user = p : publicacionesDeAux pub
                | otherwise = publicacionesDeAux pub
                where   publicador = usuarioDePublicacion p

-- describir qué hace la función: ..... Dado un usuario, devuelve las publicaciones a las que le dio like
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,ps) user = quitarRepetidos(publicacionesQueLeGustanAAux ps)
    where   publicacionesQueLeGustanAAux :: [Publicacion] -> [Publicacion]
            publicacionesQueLeGustanAAux [] = []
            publicacionesQueLeGustanAAux (p:pub)
                | pertenece user likes = p : publicacionesQueLeGustanAAux pub
                | otherwise = publicacionesQueLeGustanAAux pub
                where   likes = likesDePublicacion p

-- describir qué hace la función: ..... Dados dos usuarios, les gustan las mismas publicaciones?
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red user1 user2 = sonIguales (publicacionesQueLeGustanA red user1) (publicacionesQueLeGustanA red user2)
    where   sonIguales :: (Eq t) => [t] -> [t] -> Bool
            sonIguales l1 l2 = sonIgualesAux l1 l2 && sonIgualesAux l2 l1 && longitud l1 == longitud l2
                where   sonIgualesAux :: (Eq t) => [t] -> [t] -> Bool
                        sonIgualesAux [] _ = True
                        sonIgualesAux (x:xs) ys
                            | longitud xs == 0 = pertenece x ys
                            | otherwise = pertenece x ys && sonIgualesAux xs ys

-- describir qué hace la función: ..... Existe un usuario que le haya dado like a todas las publicaciones de otro usuario, que tienen que ser al menos una
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red user
    | longitud (publicacionesDe red user) == 0 = False
    | otherwise = cicloPorUsuario (tail (publicacionesDe red user)) (likesDePublicacion (head (publicacionesDe red user)))
    where   cicloPorUsuario :: [Publicacion] -> [Usuario] -> Bool
            cicloPorUsuario _ [] = False -- Su primera publicación no tiene likes
            cicloPorUsuario [] us = longitud us >= 1 -- Solo tiene una publicación, es true si y solo si tiene likes
            cicloPorUsuario pubs (u:us)
                | longitud us == 0 = tieneUnSeguidorFielAux pubs u
                | otherwise = tieneUnSeguidorFielAux pubs u || cicloPorUsuario pubs us
            tieneUnSeguidorFielAux :: [Publicacion] -> Usuario -> Bool
            tieneUnSeguidorFielAux (p:pub) u
                | longitud pub == 0 = pertenece u (likesDePublicacion p)
                | pertenece u (likesDePublicacion p) = tieneUnSeguidorFielAux pub u
                | otherwise = False

-- Tendría que agarrar todos los amigos de usuario 1 y fijarme con cada uno sus amigos, y así sucesivamente hasta llegar a usuario 2
-- describir qué hace la función: ..... Dada una secuencia de usuarios de la red, mayor o igual a 2, que empiece con el usuario 1 y termine con el usuario 2, hay una cadena de amigos de esa secuencia
{-existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool -- Secuencia de amigos: el usuario 1 está rel con el U1, que está rel con el U2, que está rel con el U3... que está rel con el Un, que está rel con el usuario 2, tipo dominó
existeSecuenciaDeAmigos red user1 user2 = sonIguales (amigosDe red user1) (amigosDe red user2)
    where   sonIguales :: (Eq t) => [t] -> [t] -> Bool
            sonIguales l1 l2 = sonIgualesAux l1 l2 && sonIgualesAux l2 l1 && longitud l1 == longitud l2
                where   sonIgualesAux :: (Eq t) => [t] -> [t] -> Bool
                        sonIgualesAux [] _ = True
                        sonIgualesAux (x:xs) ys
                            | longitud xs == 0 = pertenece x ys
                            | otherwise = pertenece x ys && sonIgualesAux xs ys-}

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red usuario1 usuario2 = existeSecuenciaDeAmigosAux2 red usuario1 usuario2 []
    where   existeSecuenciaDeAmigosAux2 :: RedSocial -> Usuario -> Usuario -> [Usuario] -> Bool
            existeSecuenciaDeAmigosAux2 red user1 user2 repetidos
                | pertenece user1 repetidos = False
                | pertenece (user1, user2) (relaciones red) || pertenece (user2, user1) (relaciones red) = True
                | otherwise = existeSecuenciaDeAmigosAux1 (amigosDe red user1)
                where   existeSecuenciaDeAmigosAux1 :: [Usuario] -> Bool
                        existeSecuenciaDeAmigosAux1 [] = False
                        existeSecuenciaDeAmigosAux1 (u:us)
                            | u == user1 = False
                            | otherwise = existeSecuenciaDeAmigosAux2 red u user2 (user1:repetidos) || existeSecuenciaDeAmigosAux1 us
