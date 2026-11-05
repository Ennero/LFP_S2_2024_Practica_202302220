# Sistema de inventario y movimientos en Fortran

Programa por consola que carga inventario inicial, aplica instrucciones de movimientos y genera un informe de productos. Implementado íntegramente en Fortran.

## Requisitos

GNU Fortran (gfortran). Ejecutar desde la raíz para que se encuentren inventario.inv e instrucciones.mov.

## Ejecución

Los comandos parten de la raíz del repositorio, salvo donde se indica otra terminal.

```bash
gfortran -ffree-line-length-none main.f90 -o inventario.exe
./inventario.exe
```

## Recorrido de demostración

Seleccionar cargar inventario, cargar instrucciones y crear informe, en ese orden. Revisar inventario.inv, instrucciones.mov e informe.txt para conocer los formatos de entrada y salida.

## Organización

El código y los archivos de entrada se encuentran en la raíz; los nombres de los puntos de entrada se indican arriba.

## Alcance y límites

No contiene interfaz Python ni motor general de autómatas. Los módulos .mod y el ejecutable se generan localmente.

## Contexto y licencia

Trabajo académico o de aprendizaje conservado en el portafolio de [Enner Mendizabal](https://github.com/Ennero). Los créditos de los trabajos en grupo se conservan en sus entregas y manuales.

Consultar [LICENSE](LICENSE) para la licencia de la raíz. Las dependencias y los componentes con avisos o licencias propios conservan sus condiciones.
