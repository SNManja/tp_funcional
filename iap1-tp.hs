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

--auxiliares
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e l | e == head l = True
              | otherwise = pertenece e (tail l)

longitudSinESpacios :: String -> Integer
longitudSinESpacios [] = 0
longitudSinESpacios n = if head (n) == ' ' then longitudSinESpacios (tail n) else 1 + longitudSinESpacios (tail n)


usuarioValido :: Usuario -> Bool
usuarioValido n | idDeUsuario n > 0 && longitudSinESpacios (nombreDeUsuario n) > 0 = True
                | otherwise = False

noHayIdsRepetidos :: [Integer] -> [Integer] -> Integer -> Bool
noHayIdsRepetidos x [] n = True
noHayIdsRepetidos x y n | n == head x = False
                        | null (tail x) == True && n == head x = False
                        | otherwise = if null (tail x) == True then noHayIdsRepetidos (tail y) (tail y) (head y) else noHayIdsRepetidos (tail x) y n

perteneceUsuario :: Integer -> (Integer, String) -> Bool
perteneceUsuario n t | n == fst t = True
                   | otherwise = False

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | null (usuarios (red)) == True = [] 
                      | otherwise = [nombreDeUsuario (head (usuarios (red)))] ++ nombresDeUsuarios ((tail (usuarios (red))), relaciones (red), publicaciones (red))

-- describir qué hace la función: la funcion utiliza una recursion, llamando al primer elemento de la lista de usuarios y concatenandolo al elemento siguiente.
-- Esto se logra llamando a la misma funcion, pero con la lista de usuarios sin el primer elemento. La recursion termina cuando la lista de usuarios queda vacia.

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


-- describir qué hace la función: La funcion usa 2 subfunciones, las cuales se encargan de verificar si el usuario pertenece al primer o al segundo elemento 
-- de la relacion. Esta logica de dividir la pertenencia, la utilizo en la funcion para saber si tengo que agregar a la lista de amigos al primer usuario
-- de la relacion, o al segundo. Ya que la funcion, va llamando recursivamente a todos los elementos de la lista relaciones que hay en la red, de forma
-- que si el usuario pertenece a alguna de estas relaciones, agrega a una lista al otro usuario que estaba en esa relacion.

longitudLista :: [t] -> Int
longitudLista [] = 0
longitudLista (x:xs) = 1 + longitudLista xs

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitudLista (amigosDe red u)

-- describir qué hace la función: en esta funcion, lo unico que hago, es generar la lista de amigos del usuario con la funcion anterior, y obtengo
-- la longitud de la lista.

subUsuarioConMasAmigos :: RedSocial -> Usuario -> Usuario
subUsuarioConMasAmigos red a | null (usuarios (red)) == True = a
                             | otherwise = if cantidadDeAmigos red (head (usuarios (red))) >= cantidadDeAmigos red a 
                                            then subUsuarioConMasAmigos (tail(usuarios (red)),relaciones (red),publicaciones (red)) (head (usuarios (red)))
                                                else subUsuarioConMasAmigos (tail(usuarios (red)),relaciones (red),publicaciones (red)) a

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = subUsuarioConMasAmigos (tail(usuarios (red)),relaciones (red),publicaciones (red)) (head (usuarios (red)))

-- describir qué hace la función: Esta funcion llama a una subfuncion, la cual empieza con 2 elementos(los usuarios de la red sin el primer usuario en "red", y 
-- el primer usuario de esta red en "a"). Esta subfuncion mide la cantidad de amigos del primer usuario de la lista de usuarios en red, y lo compara con a. Si el numero
-- de amigos de este usuario es mayor o igual al de a, entonces la funcion se vuelve a llamar a si misma con el tail de la lista de usuarios y con este nuevo usuario
-- reemplazando el antiguo valor de a. En el caso de que la cantidad de amigos del usuario sea menor, entonces la funcion se llama a si misma, pero a mantiene su
-- aniguo valor. De esta forma, en a siempre va a quedar guardado el usuario con mas amigos, y cuando la lista de usuarios quede vacia, la funcion devuelve el 
-- usuario que quedo almacenado en a.

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos |

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

