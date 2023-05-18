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

-- 1
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red | null (usuarios (red)) == True = [] 
                      | otherwise = [nombreDeUsuario (head (usuarios (red)))] ++ nombresDeUsuarios ((tail (usuarios (red))), relaciones (red), publicaciones (red))

-- describir qué hace la función: la funcion utiliza una recursion, llamando al primer elemento de la lista de usuarios y concatenandolo al elemento siguiente.
-- Esto se logra llamando a la misma funcion, pero con la lista de usuarios sin el primer elemento. La recursion termina cuando la lista de usuarios queda vacia.

-- 2

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

-- 3

longitudLista :: [t] -> Int
longitudLista [] = 0
longitudLista (x:xs) = 1 + longitudLista xs

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitudLista (amigosDe red u)

-- describir qué hace la función: en esta funcion, lo unico que hago, es generar la lista de amigos del usuario con la funcion anterior, y obtengo
-- la longitud de la lista.

-- 4

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

-- 5

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red | null (usuarios (red)) == True = False
                      | otherwise = if cantidadDeAmigos red (head (usuarios (red))) > 1000000 
                                        then True 
                                            else estaRobertoCarlos (tail(usuarios (red)),relaciones (red),publicaciones (red)) 

-- describir qué hace la función: el ejercicio va chequeando reursivamente todos los usuarios en la red, analizando si alguno tiene mas de 1000000 de amigos.
-- En el caso de que se cumpla, devuelve true. En el caso de que la lista de usuarios quede vacia antes de encontrar a alguien con mas de 1000000 de amigos,
-- entonces significa que nadie tiene un numero mayor de amigos que esa cantidad, por lo tanto devuelve false.

-- 6

publicacionde :: Publicacion -> Usuario -> Bool
publicacionde pub u | x1 == u = True
                    | otherwise = False
                     where (x1,x2,x3) = pub

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u | null (publicaciones (red)) == True = []
                      | otherwise = if publicacionde (head (publicaciones(red))) u == True
                                        then [head (publicaciones(red))] ++ publicacionesDe (usuarios (red),relaciones (red),tail (publicaciones (red))) u
                                            else publicacionesDe (usuarios (red),relaciones (red),tail (publicaciones (red))) u

-- describir qué hace la función: La funcion publicacionesDe, va adquiriendo recursivamente todas las publicaciones de la lista [Publicacion] que esta 
-- dentro de la red, y a traves de la subfuncion "publicacionde", se fija si el usuario guardado en "u" es autor de la publicion. En el caso de que lo sea,
-- esa publicacion es agregada a una lista, en el caso de que no lo sea, no. La recursion termina cuando el conjunto de publicaciones queda vacio, devolviendo 
-- como resultado la lista formada con l¿todas las publicaciones del usuario.

-- 7

trd (_,_,a) = a

estaDentro :: Usuario -> [Usuario]  -> Bool
estaDentro u [] = False
estaDentro u (x:xs) = if u == x then True else estaDentro u xs

publicacionesLikeadas :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesLikeadas [] _ = []
publicacionesLikeadas (x:xs) u = if estaDentro (u) (trd x)
                                     then [x] ++ publicacionesLikeadas xs u 
                                        else publicacionesLikeadas xs u

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesLikeadas (publicaciones red) u

-- describir qué hace la función: Para implemetar esta funcion usamos 2 subfunciones. "estaDentro" analiza si un usuario "u" pertenece a una lista de usuarios,
-- comparando recursivamente el usuario "u" con todos los usuarios de la lista de usuario. Si pertenece devuelve True, sino False. Luego creamos 
-- "publicacionesLikeadas" la cual toma una lista de publicaciones, y llama recursivamente a todas las publicaciones que la componen. Luego toma el tercer
-- elemento de la tripla de cada una(el tercer elemento del tipo publicaion es una lista de usuarios), y con la funcion "estaDentro", analiza si el usuario "u"
-- pertenece a los usuarios que le dieron like a la publicacion o no. En el caso de que si pertenezca, agrega la publicacion a una lista, en caso contrario, no.
-- La recursion termina cuando la lista de publicaciones queda vacia(ya se evaluaron todas las publicaciones en la lista), devolviendo la lista con todos las
-- publicaciones likeadas por el usuario. Gracias a esto, la funcion "publicacionesQueLeGustanA", unicamente tiene que llamar a la funcion anterior, poniendo
-- como valores de entrada a la lista de publicaciones de la red y al usuario a evaluar.

-- 8

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 | publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2 = True
                                          | otherwise = False
 
-- describir qué hace la función: esta funcion, analiza si la lista de publicaciones likeadas por el primer usuario es igual a la del segundo. Si lo es devuelve
-- True, sino devuelve false.
--   No hace falta analizar los casos en ue puedan tener los mismos elementos pero en distinto orden, ya que la funcion "publicacionesQueLeGustanA", va q recorrer
-- las publicaciones de la reden el mismo orden tanto para u1 como para u2. De la misma forma, los elementos de ambas listas devueltas por la funcion mencionada,
-- van a ser agregados en el mismo orden. 

-- 9

subTieneUnSeguidorFiel :: [Publicacion] -> [Usuario] -> Bool
subTieneUnSeguidorFiel publicaciones usuarios | null usuarios == True = False
                                              | publicacionesLikeadas publicaciones (head usuarios) == publicaciones = True
                                              | otherwise = subTieneUnSeguidorFiel publicaciones (tail usuarios) 

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u = subTieneUnSeguidorFiel (publicacionesDe red u) (trd (head (publicacionesDe red u)))

-- describir qué hace la función: Para esste ejercicio implementamos una subfuncion llamada "subTieneUnSeguidorFiel". Esta funcion toma como valor de entrada
-- una lista de publicaciones y una lista de usuarios. Su funcion es analizar recursivamente si alguno de estos usuarios se encuentra en la lista de likes de 
-- todas las publicaciones(para esto reutilizamos una subfuncion utilizada en el punto 7 que su funcion era ver si un usuario dio like a una publicacion). La
-- recursion termina cuando todos los usuarios de la lista ya fueron analizados(la lista de usuarios queda vacia) y devuelve False si ninguno de estos cumple
-- la condicion de estar en la lista de usuarios que dieron like de todas las publicaciones. En el caso de que alguno si lo cumpla, devuelve True.
--   Lo que hace la funcion principal es llamar a esta subfuncion, pero los valores de entrada son todas las publicaciones del usuario a analizar, y todos los
-- usuarios que le dieron like a su primera publicacion(de esta forma evitamos analizar usuarios innecesarios).

-- 10

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

