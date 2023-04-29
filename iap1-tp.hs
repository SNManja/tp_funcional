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

<<<<<<< HEAD
--auxiliares
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e l | e == head l = True
              | otherwise = pertenece e (tail l)

=======


----auxiliares----
>>>>>>> 5b576f9472b17e9a5f4f596a99cad6ee14967af5
longitud :: String -> Integer
longitud [] = 0
longitud n = if head (n) == ' ' then longitud (tail n) else 1 + longitud (tail n)

longitudLista :: [t] -> Integer
longitudLista [] = 0
longitudLista (x:xs) = 1 + longitudLista xs

usuarioValido :: Usuario -> Bool
usuarioValido n | idDeUsuario n > 0 && longitud (nombreDeUsuario n) > 0 = True
                | otherwise = False

noHayIdsRepetidos :: [Integer] -> [Integer] -> Integer -> Bool
noHayIdsRepetidos x [] n = True
noHayIdsRepetidos x y n | n == head x = False
                        | null (tail x) == True && n == head x = False
                        | otherwise = if null (tail x) == True then noHayIdsRepetidos (tail y) (tail y) (head y) else noHayIdsRepetidos (tail x) y n

<<<<<<< HEAD
=======
-- EX-2
perteneceUsuario :: Integer -> (Integer, String) -> Bool
perteneceUsuario n t | n == fst t = True
                   | otherwise = False

--redValida :: RedSocial -> Bool 
--redValida red | 

>>>>>>> 5b576f9472b17e9a5f4f596a99cad6ee14967af5
-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | null (usuarios (red)) == True = [] 
                      | otherwise = [nombreDeUsuario (head (usuarios (red)))] ++ nombresDeUsuarios ((tail (usuarios (red))), relaciones (red), publicaciones (red))

-- describir qué hace la función: .....
esElPrimero :: Usuario -> Relacion -> Bool
esElPrimero n h | n == fst h = True
              | otherwise = False
esElSegundo :: Usuario -> Relacion -> Bool
esElSegundo n h | n == snd h = True
              | otherwise = False

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u | null (relaciones (red)) == True = []
               | esElPrimero u (head (relaciones (red))) == True = [snd (head (relaciones (red)))] ++ amigosDe (usuarios(red),tail(relaciones(red)),publicaciones(red)) u
               | esElSegundo u (head (relaciones (red))) == True = [fst (head (relaciones (red)))] ++ amigosDe (usuarios(red),tail(relaciones(red)),publicaciones(red)) u
               | otherwise = amigosDe (usuarios(red),tail(relaciones(red)),publicaciones(red)) u


-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos r u = longitudLista (amigosDe r u)

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos r = usuarioConMasAmigosAux (fst r) (snd r)

usuarioConMasAmigosAux :: [Usuario] -> [Relaciones] -> Usuario
usuarioConMasAmigosAux 

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

