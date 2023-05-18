module SolucionHS where



-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrantes: 
  -- Cristal
  -- Luciano Chiariza
  -- ? 

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


{-  
    -- Ej-1 --
    En esta funcion, creamos una subfuncion que toma una lista de usuarios, agarra recursivamente el primer elemento de la lista, obtiene el segundo
    elemento de la tupla (que es el nombre del usuario), y luego, ese ultimo elemento se concatena a una lista en la cual se van a almacenar todos los 
    nombres de usuarios hasta que la lista de usuarios sea vacia.
        Luego la funcion principal se encarga de obtener la lista de usuarios de la red, y obtener la lista de nombres de usuarios utilizando la funcion
    explicada anteriormente.
-}
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios r = nombresDeUsuariosAux (usuarios r)

nombresDeUsuariosAux :: [Usuario] -> [String]
nombresDeUsuariosAux [] = []
nombresDeUsuariosAux (x:xs) = [nombreDeUsuario x] ++ nombresDeUsuariosAux xs


{-
    -- Ej-2 --
    La funcion principal obtiene el conjunto de relaciones de la red, y el usuario el cual queremos ver los amigos. Dicha funcion auxiliar,
    se encarga de ver todas las relaciones dentro del conjunto de relaciones, y por cada una se fija si alguno de los dos elementos de la tupla es el 
    usuario (el cual queremos ver sus amigos). En el caso de que alguno de estos elementos sea el usuario, entonces agrega al otro usuario a una 
    lista en la cual se van a almacenar todos los amigos.
-}
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe r u = amigosDeAux (relaciones r) u 

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] _ = []
amigosDeAux (rh:rt) u | fst rh == u = (snd rh:amigosDeAux rt u)
                      | snd rh == u = (fst rh:amigosDeAux rt u)
                      | otherwise = amigosDeAux rt u 


{-
    -- Ej-3 --
     En esta funcion, reciclamos la funcion hecha anteriormente(amigosDe), gracias a la cual, extraemos la lista de amigos del usuario a analizar, y con
     la subfuncion llamada "logitudLista", contamos los elementos de dicha lista para obtener la cantidad de amigos.
-}
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos r u = longitudLista (amigosDe r u)

longitudLista :: [t] -> Integer
longitudLista [] = 0
longitudLista (x:xs) = 1 + longitudLista xs 

{-
    -- Ej-4 --
     Esta funcion llama a una subfuncion, la cual empieza con 2 elementos(los usuarios de la red sin el primer usuario en "red", y 
    el primer usuario de esta red en "a"). Esta subfuncion mide la cantidad de amigos del primer usuario de la lista de usuarios en red, y lo compara con a. Si el numero
    de amigos de este usuario es mayor o igual al de a, entonces la funcion se vuelve a llamar a si misma con el tail de la lista de usuarios y con este nuevo usuario
    reemplazando el antiguo valor de a. En el caso de que la cantidad de amigos del usuario sea menor, entonces la funcion se llama a si misma, pero a mantiene su
    aniguo valor. De esta forma, en a siempre va a quedar guardado el usuario con mas amigos, y cuando la lista de usuarios quede vacia, la funcion devuelve el 
    usuario que quedo almacenado en a.
-}
subUsuarioConMasAmigos :: RedSocial -> Usuario -> Usuario
subUsuarioConMasAmigos red a | null (usuarios (red)) == True = a
                             | otherwise = if cantidadDeAmigos red (head (usuarios (red))) >= cantidadDeAmigos red a 
                                            then subUsuarioConMasAmigos (tail(usuarios (red)),relaciones (red),publicaciones (red)) (head (usuarios (red)))
                                                else subUsuarioConMasAmigos (tail(usuarios (red)),relaciones (red),publicaciones (red)) a

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = subUsuarioConMasAmigos (tail(usuarios (red)),relaciones (red),publicaciones (red)) (head (usuarios (red)))
{-
    -- Ej-5 --
    Este ejercicio se resuelve simplemente con el uso de la funcion usuarioConMasAmigos recien desarrollada y viendo al cantidadDeAmigos del mismo, lo podemos comparar con el valor numerico 10
-}
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos rs | usuarios(rs) == [] = False
                     | cantidadDeAmigos rs (head(usuarios(rs))) >= 10 = True
                     | otherwise = estaRobertoCarlos ((tail(usuarios(rs))), relaciones(rs), publicaciones(rs))
 
{-
     -- Ej 6 --
    La función principal extrae las lista de publicaciones de la red social para que pueda ser usada por su auxiliar.
    La función auxiliar analiza recursivamente si el primer usuario de la lista de publicaciones es igual al ingresado por parametro hasta llegar al
    final de la lista devolviendo una lista de publicaciones pertenecientes a dicho usuario.
-}
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe r u = publicacionesDeAux (publicaciones r) u 

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] u = []
publicacionesDeAux (ph:pt) u | usuarioDePublicacion(ph) == u = [ph] ++ publicacionesDeAux pt u
                             | otherwise = publicacionesDeAux pt u
{-
    
    -- Ej 7 --

    La función principal extrae la lista de publicaciones de la red social para que pueda ser usada por su auxiliar.
    En la función auxiliar se determina recursivamente hasta llegar al final de la lista con la función publicacionLikeada si el usuario
    ingresado por parametro pertenece a la lista de likes de la primera publicación devolviendo una lista con las publicaciones que le gustan
    al usuario ingresado.

-}
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAAux (publicaciones r) u

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] u = []
publicacionesQueLeGustanAAux (ph:pt) u | publicacionLikeada (likesDePublicacion ph) u = [ph] ++ publicacionesQueLeGustanAAux pt u
                                       | otherwise = publicacionesQueLeGustanAAux pt u

publicacionLikeada :: [Usuario] -> Usuario -> Bool
publicacionLikeada [] u = False
publicacionLikeada (x:xs) u | x == u = True
                            | otherwise = publicacionLikeada xs u

{-
    -- Ej-8 --

        En esta funcion, reutilizamos la funcion del punto anterior para ver que el conjunto de publicaciones que les gustan a los dos usuarios 
    sean iguales. Debido a que recorremos el conjunto de publicaciones de la red en el mismo orden para los dos usuarios, si les gustan
    las mismas publicaciones, entonces los conjuntos de publicaciones gustadas van a ser iguales.
-}
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = (publicacionesQueLeGustanA r u1 == publicacionesQueLeGustanA r u2) && (publicacionesQueLeGustanA r u1 /= [])


{-
    -- Ej-9 --
        en esta funcion, utilizamos 2 subfunciones auxiliares. "tieneUnSeguidorFielAux" recibe una lista de usuarios y una lista de publicaciones,
    y pasando recursivamente por todos los usuarios, verifica que alguno haya dado like a todo el conjunto de publicaciones, a traves
    de la otra subfuncion llamada "likeoTodas". Esta segunda subfuncion, recibe un conjunto de publicaciones y un usuarios, y se encarga de
    pasar por toda la lista de publicaciones recursivamente, verificando que el usuario haya dado like a todas las publicaciones.
        Gracias a estas dos subfunciones, la funcion principal unicamente tiene que llamar a "tieneUnSeguidorFielAux", dandole como
    valores de entrada los usuarios de la red y las publicaciones del usuario a analizar(esto lo hacemos con la funcion "publicacionesDe"
    que hicimos en el punto 6).

-}
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u = tieneUnSeguidorFielAux (usuarios r) (publicacionesDe r u) 


-- Aca entraria           todos user -> publis del q analisamos -> true/false
tieneUnSeguidorFielAux :: [Usuario] -> [Publicacion] -> Bool
tieneUnSeguidorFielAux _ [] = False -- Esto elimina el true en caso que no tenga publicaciones
tieneUnSeguidorFielAux [] p = False
tieneUnSeguidorFielAux (uh:ut) p | likeoTodas p uh = True
                                 | otherwise = tieneUnSeguidorFielAux ut p

likeoTodas :: [Publicacion] -> Usuario -> Bool
likeoTodas [] _ = True
likeoTodas (ph:pt) u | publicacionLikeada (likesDePublicacion ph) u = likeoTodas pt u
                     | otherwise = False


{-
    -- Ej-10 --

        El ejercicio esta planteado desde la perspectiva de que las amistades se abren como un arbol.
        Este entraria en loop eterno si es que no tenemos una lista donde guardar los elementos del arbol que ya fueron analizados. Para eso se usa la variable vistos.

        BuscoPor checkea si es la lista de amigos de un usuario cualquiera esta el que queremos encontrar (el que entra en existeSecuenciaDeAmigos como u2, llamemoslo u* para esta explicacion)
        En caso que no encuentre al u*, se llamda a "buscoPorCadaAmigo" sacando a los elementos ya vistos de la lista de amigos

        buscoPorCadaAmigo tiene como fin agarrar una lista de amigos y hacer que buscoPor analice los amigos de cada amigo de la lista de amigos. 
        Podemos ver que en caso de que no de falso, el otherwise nos manda al llamado de buscoPor (analizando los amigos de cada amigo de la lista de amigos) 
        y a su ves se llama a si misma, conectada por un ||, de esta manera se llamada recursivamente con cada elemento de la lista (ah:at) que seria la lista amigos.

        Como todo esta conectado por un || en caso que nos de TRUE en solo un procedimiento, nos da TRUE todo el statement.
        En resumen podriamos decir que buscoPor es la funcion encargada de analizar cada caso y buscoPorCadaAmigo la que se encarga de llamar recursivamente cada caso (atravezar todo el arbol)

        Luego se peude ver que esta la listaASinB y pertenece
        Estas funciones me permiten:
        - listaASinB : Saca a los elementos de la lista B que esten tambien en la lista A -> por ej 
            -- A = [1,2,3,4] ; B = [1,4] -> listaASinB A B  -> [2,3]
            El fin de esta funcion en el ejercicio es poder eliminar los usuarios en la lista de vistos (B) de los amigos que vamos a analizar (A)
            Gracias a esta funcion no se hace infinito el llamado recursivo
        - pertenece : Checkea si un usuario pertenece a una lista especifica
            -- u = (20, "juan") lista = [(20, "juan"), (21, "gustavo")] -> pertenece u lista -> True
            Con este predicado checkeamos si en buscoPor se encontro al u*, devolviendo true en el caso que se cumpla

-}
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 =  buscoPor r u1 (amigosDe r u2) [u2] 

buscoPor :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
buscoPor r u amigos vistos | listaASinB amigos vistos == [] = False
                           | pertenece u amigos = True 
                           | otherwise = buscoPorCadaAmigo r u (listaASinB amigos vistos) (vistos)

buscoPorCadaAmigo :: RedSocial ->  Usuario -> [Usuario] -> [Usuario] -> Bool
buscoPorCadaAmigo r u [] vistos = False
buscoPorCadaAmigo r u (ah:at) vistos = buscoPor r u (amigosDe r ah) (vistos ++ [ah]) || buscoPorCadaAmigo r u at (vistos++[ah])

listaASinB :: [Usuario] -> [Usuario] -> [Usuario]
listaASinB [] _ = []
listaASinB a [] = a
listaASinB (ah:at) b = if pertenece ah b then listaASinB at b else ([ah] ++ listaASinB at b)


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs) | x == t = True
                   | otherwise = pertenece t xs


-- Estos son predicados utilizados meramente para pensar el ejercicio. Pueden ser eliminados y todo funcionaria bien.
cadenaDeAmigos :: RedSocial -> [Usuario] -> Bool
cadenaDeAmigos r (uh:ut) | null ut  || null (uh:ut)= True
                         | relacionadosDirecto r uh (head ut) = cadenaDeAmigos r ut
                         | otherwise = False

relacionadosDirecto :: RedSocial -> Usuario -> Usuario ->  Bool
relacionadosDirecto r u1 u2 = pertenece u1 (amigosDe r u2)
