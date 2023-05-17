module TestSolucion where

import Test.HUnit
import SolucionHS





main = runTestTT tests

tests = test [
    -- Los casos que me interesan testear son 2: Cuando tengo Usuarios en mi Red y cuando no
    " nombresDeUsuarios Default" ~: (nombresDeUsuarios redA) ~?= ["Solin solito","Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios RedVacia" ~: (nombresDeUsuarios redSinUsuarios) ~?= [],

    {-
        los casos que nos interesan son 3: 
         el caso en el que un usuario tiene mas de un amigo (y de paso probamos que funcione cuando el usuario esta primero en la tupla, y cuando esta segundo)
         despues probamos el caso borde, que es cuando un usuario tiene un solo amigo
         y luego probamos cuando un usuario no tiene ningun amigo
    -}
    " amigosDe Default" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " amigosDe RedVacia" ~: (amigosDe redA usuario0) ~?= [],
    " amigosDe 1amigo" ~: (amigosDe redB usuario1) ~?= [usuario2],

    -- Los casos que me interesan son 2:
    -- Cuando el usuario no tiene amigos,
    -- Cuando el usuario tiene uno o mas amigos
    " cantidadDeAmigos ninguno" ~: (cantidadDeAmigos redA usuario0) ~?= 0,
    " cantidadDeAmigos alguno" ~: (cantidadDeAmigos redA usuario1) ~?= 2,


    --los casos que nos interesan en esta funcion son 4:
    --    cuando 2 personas tienen la misma cantidad de amigos y al mismo tiempo son lo que mas amigos tienen
    --    cuando 1 persona tiene mas amigos que el resto 
    --    cuando ningun usuario de la red tiene amigos
    " usuarioConMasAmigos 2MismaCantAmigos" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    " usuarioConMasAmigos 1ConMasAmigos" ~: (usuarioConMasAmigos redB) ~?= usuario2,
    " usuarioConMasAmigos redSinAmigos" ~: expectAny (usuarioConMasAmigos redSinAmistades) usuariosA,
    


    -- los casos quenos interesan en esta funcion son 4:
    --    cuando un usuario no tiene amigos
    --    cuando un usuario tiene menos de 10 amigos
    --    cuando un usuario tiene 10 amigos
    --    cuando un usuario tiene mas de 10 amigos
    " NOestaRobertoCarlos antisocial" ~: (estaRobertoCarlos redSinAmistades) ~?= False,
    " NOestaRobertoCarlos " ~: (estaRobertoCarlos redA) ~?= False,
    " estaRobertoCarlos justo (10 amigos)" ~: (estaRobertoCarlos redRobertoCarlos10) ~?= True, 
    " estaRobertoCarlos pasado (12 amigos)" ~: (estaRobertoCarlos redRobertoCarlos12) ~?= True, 
    

    {-
        en esta funcion nos interesan 3 casos: 
        el caso borde en el que el usuario tiene solo 1 publicacion
        el caso en el que tiene mas de 1 publicacion
        el caso en el que no tiene publicaciones    
    -}
    " publicacionesDe Con1Publi" ~: (publicacionesDe redC usuarioC_1) ~?= [publicacionC_1],
    " publicacionesDe ConMasde1Publis" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
    " publicacionesDe SinPublis" ~: (publicacionesDe redA usuario0) ~?= [],

    -- en esta funcion nos interesan 3 casos:
    --    el caso borde en el que le gusta 1 sola publicacion
    --    el caso en el que le gustan mas de 1 publicacion
    --    el caso en el que no le gusta ninguna publicacion
    " publicacionesQueLeGustanA Una" ~: (publicacionesQueLeGustanA redA_2 usuario4) ~?= [publicacion1_2],
    " publicacionesQueLeGustanA MasDeUna" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
    " publicacionesQueLeGustanA ninguna" ~: (publicacionesQueLeGustanA redA usuario0) ~?= [],


    -- es esta funcion nos interesan 5 casos:
    -- !   el caso en el que a los dos usuarios les gustan las mismas publicaciones
    -- !   el caso en el que a los dos usuarios les gustan publicaciones diferentes
    -- !   el caso en el que al primero usuario les gustan publicaciones y al segundo ninguna
    -- !   el caso en el que al segundo usuario le gustan publicaciones y al primero ninguna
    -- !   el caso en el que al primer usuario no le gusta ninguna publicacion y al segundo tampoco
    " lesGustanLasMismasPublicaciones True" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
    " lesGustanLasMismasPublicaciones False" ~: (lesGustanLasMismasPublicaciones redA usuario1 usuario2) ~?= False,


    -- en esta funcion no interesan 4 casos:
    --    el caso en el que el usuario tenga un usuario que le de like a todas sus plublicaciones
    --    el caso en el que el usuario tenga publicaciones con like, pero que ninguno de esos usuarios que dieron like sea un "seguidor fiel"(que le haya dado like a todas las publicaciones)
    --    el caso en el que el usuario tenga publicaciones sin likes
    --    el caso en el que el usuario no tenga publicaciones
    
    " tieneUnSeguidorFiel ConSeguidorFiel" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    " tieneUnSeguidorFiel SinSeguidorFiel" ~: (tieneUnSeguidorFiel redA usuario3) ~?= False,
    " tieneUnSeguidorFiel SinLikes" ~: (tieneUnSeguidorFiel redSeguidorFielSinLikes usuariosD_1) ~?= False,
    " tieneUnSeguidorFiel SinPublis" ~: (tieneUnSeguidorFiel redA usuario0) ~?= False,

    -- en esta funcion nos interesan 5 casos:
    --    el caso en el que los 2 usuarios son amigos directos
    --    el caso en el que si existe una red de amigos entre  los 2 usuarios
    --    el caso en el que el primer usuario no tiene amigos y el segundo si
    --    el caso en el que el segundo usuario no tiene amigos y el segundo si
    --    el caso en el que no exite una red de amigos, pero que los 2 usuarios tengan amigos

    " existeSecuenciaDeAmigos DirectamenteAmigos" ~: (existeSecuenciaDeAmigos redC usuarioC_1 usuarioC_3) ~?= True,
    " existeSecuenciaDeAmigos SonAmigosDeAmigos..." ~: (existeSecuenciaDeAmigos redC usuarioC_1 usuarioC_2) ~?= True,
    " existeSecuenciaDeAmigos U1 sin amigos" ~: (existeSecuenciaDeAmigos redC usuarioC_5 usuarioC_1) ~?= False,
    " existeSecuenciaDeAmigos U2 sin amigos" ~: (existeSecuenciaDeAmigos redC usuarioC_1 usuarioC_5) ~?= False,
    " existeSecuenciaDeAmigos CadenaExtensaNoChocan" ~: (existeSecuenciaDeAmigos redAmixC usuarioC_1 usuario1) ~?= False

 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario0 = (0, "Solin solito")
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


usuariosA = [usuario0, usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA :: ([(Integer, String)], [((Integer, String), (Integer, String))],
 [((Integer, String), String, [(Integer, String)])])
redA = (usuariosA, relacionesA, publicacionesA)

redA_2 = (usuariosA, relacionesA, [publicacion1_2])

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)


--Esta red esta elaborada con fines de testear el ultimo ejercicio antes de que se haya dado la clase de HUnit. 
--Por eso esta expresada diferente. Especificamente el caso de la redC nos muestra cuando 
-- Hay una cadena de amigos de amigos entre -> 20 juan manuel y 23 pedro montes
-- Son amigos directos -> 20 Juan Manuel y 30 Juana Alvarez
-- Uno tiene amigos y el otro no -> 20 juan manuel y 70 Marcos Dominguez (siendo marcos el solitario)
usuarioC_1 = (20, "Juan Manuel")
usuarioC_2 = (23, "Pedro Montes")
usuarioC_3 = (30, "Juana Alvarez")
usuarioC_4 = (55, "Silvia Rodriguez")
usuarioC_5 = (70, "Marcos Dominguez")

relacionC_1 = ((20, "Juan Manuel"), (30, "Juana Alvarez"))
relacionC_2 = ((30, "Juana Alvarez"), (55, "Silvia Rodriguez"))
relacionC_3 = ((55, "Silvia Rodriguez"),(23, "Pedro Montes"))

publicacionC_1 = ((20, "Juan Manuel"), "Hola mundo",[(30, "Juana Alvarez"),(70, "Marcos Dominguez")])

usuariosC = ([usuarioC_1, usuarioC_2, usuarioC_3, usuarioC_4, usuarioC_5])
relacionesC = [relacionC_1, relacionC_2, relacionC_3]
publicacionesC = [publicacionC_1]


redC = (usuariosC, relacionesC, publicacionesC)

redAmixC = (usuariosA ++ usuariosC, relacionesA ++ relacionesC, publicacionesA ++ publicacionesC)




redSinUsuarios = ([],[],[])
redSinAmistades = (usuariosA, [], publicacionesA)

usuariosD_1 = (1,"a")
usuariosD_2 = (2,"b")
usuariosD_3 = (3,"c")
usuariosD_4 = (4,"d")
usuariosD_5 = (5,"e")
usuariosD_6 = (6,"f")
usuariosD_7 = (7,"g")
usuariosD_8 = (8,"h")
usuariosD_9 = (9,"i")
usuariosD_10 = (10,"j")
usuariosD_11 = (11,"k")
usuariosD_12 = (12,"l")
usuariosD_13 = (13,"m")

relacionD_1_2 =((1,"a"),(2,"b"))
relacionD_1_3 =((1,"a"),(3,"c"))
relacionD_1_4 =((1,"a"),(4,"d"))
relacionD_1_5 =((1,"a"),(5,"e"))
relacionD_1_6 =((1,"a"),(6,"f"))
relacionD_1_7 =((1,"a"),(7,"g"))
relacionD_1_8 =((1,"a"),(8,"h"))
relacionD_1_9 =((1,"a"),(9,"i"))
relacionD_1_10 =((1,"a"),(10,"j"))
relacionD_1_11 =((1,"a"),(11,"k"))
relacionD_1_12 =((1,"a"),(12,"l"))
relacionD_1_13 =((1,"a"),(13,"m"))

usuariosD = [usuariosD_1,usuariosD_2,usuariosD_3,usuariosD_4,usuariosD_5,usuariosD_6,usuariosD_7,usuariosD_8,usuariosD_9,usuariosD_10,usuariosD_11,usuariosD_12,usuariosD_13]
relacionesD12 = [relacionD_1_2, relacionD_1_3, relacionD_1_4,relacionD_1_5,relacionD_1_6,relacionD_1_7,relacionD_1_8,relacionD_1_9,relacionD_1_10,relacionD_1_11,relacionD_1_12,relacionD_1_13]
relacionesD10 = [relacionD_1_2, relacionD_1_3, relacionD_1_4,relacionD_1_5,relacionD_1_6,relacionD_1_7,relacionD_1_8,relacionD_1_9,relacionD_1_10,relacionD_1_11]
redRobertoCarlos12 = (usuariosD,relacionesD12,[])
redRobertoCarlos10 = (usuariosD,relacionesD10,[])
redSeguidorFielSinLikes = (usuariosD,relacionesD10, [((1,"a"),"test",[])])



