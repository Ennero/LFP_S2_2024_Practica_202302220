module globales
    implicit none
    character(len=50), dimension (3, 50) :: datos !Declarando el arreglo que contiene TODO el archivo
    real, dimension(3,50) :: datosN
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
        print *, "#3. Crear informa de inventario"
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

subroutine cargar_inventario()
    use globales
    implicit none
    !Declarando las variables para la carag del inventario
    character(len=50) :: linea,nombre, instruccion, ubicacion, parametros
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
                    read(parametros(1:p-1), '(F6.2)', iostat=iostat) precio
                    ubicacion=parametros(p+1:)
                end if
                !print *,"nombre:",nombre,"cantidad:",cantidad,"precio:",precio,"ubicacion:",ubicacion
                datos(1,contador)=instruccion !Guardando los datos en el arreglo
                datos(2,contador)=nombre
                datosN(2,contador)=cantidad
                datosN(3,contador)=precio
                datos(3,contador)=ubicacion
                datosN(1,contador)=contador
                print *, "intruccion:" ,datos(1,contador),"nombre:",datos(2,contador),"cantidad:",datosN(1,contador),"precio:",datosN(2,contador),"ubicacion:",datos(3,contador)
            else
                    print *, "Instruccion", instruccion ," de la linea", contador, " no valida"
            end if
        end do
        print *, "Se han cargado ",exitos, " productos al inventario exitosamente"
        close(1)
    else
        print *, "El archivo inventario.inv no existe"
        return
    end if
end subroutine cargar_inventario !Terminando la funcion para leer el archivo

















subroutine cargar_instrucciones()
    use globales
    implicit none
    
end subroutine cargar_instrucciones !Terminando la funcion para leer el archivo

subroutine crear_informe()
    use globales
    implicit none
    
end subroutine crear_informe !Terminando la funcion para leer el archivo

