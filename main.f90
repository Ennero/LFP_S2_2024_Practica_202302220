module globales
    implicit none
    character(len=15), dimension (3, 500) :: datos !Declarando el arreglo que contiene TODO el archivo
    real, dimension(3,500) :: datosN
    integer :: productos !Declarando la variable que contiene la cantidad de productos que
contains
end module globales

program sistema_inventario
    use globales
    implicit none
    logical :: Salir !Declarando las Variables
    integer :: opcion
    salir=.false.
    print *, "Bienvenido al sistema de inventario :)" !Bienvenida
    do while (.not. Salir) !Iniciando el ciclo
        print *, "SELECCIONE UNA OPCION:"
        print *, "#1. Cargar inventario inicial"
        print *, "#2. Cargar Instrucciones de movimiento"
        print *, "#3. Crear informe de inventario"
        print *, "#4. Salir"
        read *, opcion
        select case(opcion)
            case(1)
                call cargar_inventario()
            case(2)
                call cargar_instrucciones()
            case(3)
                call crear_informe()
            case(4)
                salir = .true.
                print *, "Saliendo del programa..."
            case default
                print *, "Opcion no valida"
        end select
    end do
end program sistema_inventario

subroutine cargar_inventario() !Funcion para leer el archivo
    use globales
    implicit none
    !Declarando las variables para la carag del inventario
    character(len=50) :: linea,parametros
    character(len=15) :: nombre, ubicacion, instruccion
    integer:: cantidad, iostat, contador,p,exitos
    real :: precio
    logical :: e
    contador=0
    exitos=0
    !Verifico la existencia del archivo
    inquire(file="inventario.inv", exist=e)
    if(e) then
        !Abriendo el archivo
        open(unit=1, file="inventario.inv", status="old", action="read")
        do
            read(1, '(A)', iostat=iostat) linea! Leyendo cada línea   
            if(iostat/=0) then 
                exit
            end if
            contador=contador+1
            p=index(linea, ' ')!Separando la instrucción de los parámetros
            if(p>0) then
                read(linea(1:p-1), '(A)', iostat=iostat) instruccion
                parametros=linea(p+1:)
            end if
            if(instruccion=="crear_equipo") then
                exitos=exitos+1
                p=index(parametros, ';')!Separando el nombre de los demás parámetros
                if(p>0) then
                    read(parametros(1:p-1), '(A)', iostat=iostat) nombre
                    parametros=parametros(p+1:)
                end if
                p=index(parametros, ';')!Separando la cantidad de los demás parámetros
                if(p>0) then
                    read(parametros(1:p-1), '(I6)', iostat=iostat) cantidad
                    parametros=parametros(p+1:)
                end if
                p=index(parametros, ';')!Separando el precio de la ubicación
                if(p>0) then
                    read(parametros(1:p-1), '(F6.2)', iostat=iostat) precio !El precio debe estar en formato 6.2 [dos decimales]
                    ubicacion=parametros(p+1:)
                end if
                !print *,"nombre:",nombre,"cantidad:",cantidad,"precio:",precio,"ubicacion:",ubicacion
                datos(1,exitos)=instruccion !Guardando los datos en el arreglo
                datos(2,exitos)=nombre
                datosN(2,exitos)=cantidad
                datosN(3,exitos)=precio
                datos(3,exitos)=ubicacion
                datosN(1,exitos)=contador
                print *, "INSTRUCCION: " ,trim(datos(1,exitos)),"; NOMBRE: ",trim(datos(2,exitos)),"; CANTIDAD: ",datosN(2,exitos),"; PRECIO: ",datosN(3,exitos),"; UBICACION: ",trim(datos(3,exitos))
            else
                    print *, "INSTRUCCION: ", instruccion ," de la linea", contador, " no valida"
            end if
        end do
        print *, "SE HAN CARGADO ",exitos, " PRODUCTOS AL INVENTARIO EXITOSAMENTE"
        close(1)
        productos=exitos
    else
        print *, "EL ARCHIVO INVENTARIO.INV NO EXISTE"
        return
    end if
end subroutine cargar_inventario !Terminando la funcion para leer el archivo

subroutine cargar_instrucciones() !Funcion para cargar las instrucciones
    use globales
    implicit none
    character(len=50) :: linea,nombre, instruccion, ubicacion, parametros
    integer:: cantidad, iostat, contador,p,exitos,i,pos
    logical :: e,v1
    contador=0
    exitos=0
    !Verifico la existencia del archivo
    inquire(file="instrucciones.mov", exist=e)
    if(e) then
        !Abriendo el archivo
        open(unit=1, file="instrucciones.mov", status="old", action="read")
        do
            read(1, '(A)', iostat=iostat) linea! Leyendo cada línea   
            if(iostat/=0) then 
                exit
            end if
            contador=contador+1
            p=index(linea, ' ')!Separando la instrucción de los parámetros
            if(p>0) then
                read(linea(1:p-1), '(A)', iostat=iostat) instruccion
                parametros=linea(p+1:)
            end if
            if(instruccion=="agregar_stock") then !PARA AGREGAR STOCK--------------------------------------------------
                p=index(parametros, ';')!Separando el nombre de los demás parámetros
                if(p>0) then
                    read(parametros(1:p-1), '(A)', iostat=iostat) nombre
                    parametros=parametros(p+1:)
                end if
                p=index(parametros, ';')!Separando la cantidad de los demás parámetros
                if(p>0) then
                    read(parametros(1:p-1), '(I6)', iostat=iostat) cantidad
                    ubicacion=parametros(p+1:)
                end if
                do while (i<500) !Ciclo para recorrer el arreglo y aumentar la cantidad
                    i=i+1
                    if (nombre==datos(2,i)) then
                        if ( ubicacion==datos(3,i) ) then
                            datosN(2,i)=datosN(2,i)+cantidad
                            exitos=exitos+1
                            v1=.true.
                            pos=i
                        else
                            v1=.false.
                            print *,"ERROR, el producto ", trim(nombre) ," no se encuentra en ", trim(ubicacion)
                        end if
                    else
                        end if
                end do !COLOCAR UN BANDERA PARA PODER SABER SI SE MANTUVO IGUAL O SE MODIFICO
                i=0 !Reinicio de la cuenta del ciclo
                if(v1) then
                    print *, "Producto: ", trim(nombre), " actualizo su cantidad a: ", datosN(2,pos)
                end if
            else
                if(instruccion=="eliminar_equipo") then !PARA ELIMINAR EQUIPO--------------------------------------------------
                    p=index(parametros, ';')!Separando el nombre de los demás parámetros
                if(p>0) then
                    read(parametros(1:p-1), '(A)', iostat=iostat) nombre
                    parametros=parametros(p+1:)
                end if
                p=index(parametros, ';')!Separando la cantidad de los demás parámetros
                if(p>0) then
                    read(parametros(1:p-1), '(I6)', iostat=iostat) cantidad
                    ubicacion=parametros(p+1:)
                end if

                do while (i<500) !Ciclo para recorrer el arreglo y eliminar una cantidad
                    i=i+1
                    if (nombre==datos(2,i)) then
                        if (ubicacion==datos(3,i) ) then
                            if (cantidad>datosN(2,i)) then
                                print *,"ERROR, la cantidad a eliminar del producto", trim(nombre), " es mayor a la cantidad en stock"
                                v1=.false.
                            else
                            datosN(2,i)=datosN(2,i)-cantidad
                            exitos=exitos+1
                            v1=.true.
                            pos=i
                            end if
                        else
                            v1=.false.
                            print *,"ERROR, el producto ", trim(nombre) ," no se encuentra en ", trim(ubicacion)
                        end if
                    else
                        end if
                end do
                i=0 !Reinicio de la cuenta del ciclo
                if(v1) then
                    print *, "Producto: ", trim(datos(2,pos)), " actualizo su cantidad a: ", datosN(2,pos)
                end if                
                else
                    print *, "Instruccion: ", trim(instruccion) ," de la linea", contador, " no valida"
                end if
            end if
        end do
            print *, "Se ha actualizado la cantidad de ",exitos, " productos exitosamente"
        close(1)
    else
        print *, "El archivo instrucciones.mov no existe"
        return
    end if
end subroutine cargar_instrucciones !Terminando la funcion para leer el archivo

subroutine crear_informe() !función para crear el informe
    use globales
    implicit none
    character(len=15) :: informe
    integer :: i
    i=0
    informe="informe.txt"
    open(unit=22, file=informe,status="unknown", action="write") !Abriendo el archivo y creandolo
    write(22,*) "-------------------------| INFORME DE INVENTARIO |-------------------------" !Escribiendo en el archivo
    write(22,*) "|    EQUIPO    |   CANTIDAD   |  PRECIO UNITARIO  | VALOR TOTAL | UBICACION |"
    do while(i<productos) !Ciclo para recorrer el arreglo y escribir en el archivo
        i=i+1
        write(22,*) datos(2,i), datosN(2,i), datosN(3,i), datosN(2,i)*datosN(3,i), trim(datos(3,i))
    end do
    write(22,*) "---------------------------------------------------------------------------" !Mensaje de confirmación
    close(22); !Cerrando el archivo
    print *, "Informe generado exitosamente :)" !Mensaje de confirmación
end subroutine crear_informe !Terminando la funcion para leer el archivo

