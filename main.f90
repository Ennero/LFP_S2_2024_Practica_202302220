module globales
    implicit none
    character, dimension(5:50) :: datos !Declarando el arreglo que contiene TODO el archivo

contains
    
end module globales

program sistema_inventario
    use globales
    implicit none
    logical :: Salir !Declarando las Variables
    integer :: opcion
    salir=.false.
    print *, "#Bienvenido al sistema de inventario" !Bienvenida
    do while (.not. Salir) !Iniciando el ciclo
        print *, "#SELECCIONE UNA OPCION:"
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
    character(len=256) :: linea
    character(len=192) :: nombre, instruccion, ubicacion
    integer:: cantidad, iostat, contador
    real :: precio
    logical :: e
    contador=0
    !Verifico la existencia del archivo
    inquire(file="inventario.inv", exist=e)
    if(e) then
        !Abriendo el archivo
        open(unit=1, file="inventario.inv", status="old", action="read")
        do
            read(1, '(A)', iostat=iostat) linea
            if(iostat/=0) exit
            !Mostrando el contenido del archivo
            read(linea,*, iostat=iostat) instruccion, nombre, cantidad, precio, ubicacion
            print *, "Producto: ", nombre, "Cantidad: ", cantidad, "Precio: ", precio, "Ubicacion: ", ubicacion
            contador=contador+1
        end do
        print *, "Se han cargado ", contador, " productos al inventario exitosamente"
        close(1)
    else
        print *, "El archivo inventario.inv no existe"
        return
    end if


    
end subroutine cargar_inventario !Terminando la funcion para leer el archivo


