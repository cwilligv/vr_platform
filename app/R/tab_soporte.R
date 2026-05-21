soporte_ui <- function(id){
  tabItem(
    tabName = "tab7",
    h1("Bienvenido a la plataforma de Capacitaciones 3d de MERC", style = "font-size: 1.8rem;"),
    bs4Dash::box(
      width = 12,
      headerBorder = F,
      collapsible = F,
      fluidRow(
        markdown(
          "
            ## Para comenzar
            Comience eligiendo un **módulo** del menú ubicado al lado izquierdo.
            *****
            ### Los Módulos
            Los módulos que provee la plataforma tienen como objetivo facilitar el manejo de las preparaciones que MERC realiza. En
            la versión mas actual de la plataforma existen los siguientes módulos:
            1. **Inicio**: Despliega un resumen de las capacitaciones realizadas tanto históricas como en el mes actual.
            2. **Inscripciones**: Permite inscribir participantes que requieren capacitaciones.
            3. **Monitor**: Permite monitorear el estado de avance de las capacitaciones.
            4. **Resultados**: Permite subir planilla con los resultados de las pruebas para visualizar los resultados.
            5. **Facturas**: Permite acceder al estado de pago de las facturas emitidas.
            
            *****
            ### Módulo Inscripciones
            En este módulo podras inscribir a las personas que requieres que sean preparadas. Debes completar todos los campos con asteriscos.
            El sistema asignará las fechas de preparación para el día anterior a la fecha online/presencial. Si no se encuentra disponibilidad 
            entonces nos contactaremos contigo para coordinar una fecha y hora de preparación.
            
            ![ingreso de participantes][ui_participantes]
            
            [ui_participantes]:images/ui_inscripciones.png
            
            Para editar los datos de los participantes que hayas inscrito solo debes seleccionar a la persona y luego apretar el botón de Editar.
            *****
            ### Módulo Monitor
            En este modulo podrás monitorear el estado en el que se encuentra la preparación a los participantes que has inscrito. Estos pueden ser: 
            Incrito, Capacitado, Inasistente, Cancelado. En caso que el estado sea cancelado, tendras que reagendar nuevamente.
            *****
            ### Módulo Resultados
            En este modulo podrás subir los resultados de las pruebas y ver el detalle de una manera más rápida y fácil.
            *****
            ### Módulo Facturas
            En este modulo podrás acceder al estado de las facturas emitidas.
            *****
            ### Desarrollo
            Nuestra plataforma ha sido testeada con las versiones mas recientes de Chrome, por lo que recomendamos usar ese navegador.
            Internet Explorer no esta completamente soportado por nuestra plataforma. Cualquier problema solo escribemos.
            Estamos en continuo desarrollo por lo que cualquier feedback que desees darnos es BIENVENIDO.
            
            Visita nuestro sitio web [nuestro sitio](https://www.mercconsultora.cl) para mas detalles!
            
            ") %>%
          div(class = "sps-dash"),
        spsHr(),
        br(),
        br()
      )
    )
  )
}