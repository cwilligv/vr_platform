envio_email_participante <- function(cita, dest, nombre_dest, emails_cc, emails_bcc, razon_social, solicitante, cargo_solicitante, telefono_solicitante, email_solicitante, rut, prep_id, razon, asunto){
  
  subject <- asunto # "CAPACITACIÓN"
  
  body <- mime_part(glue('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
          Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
          <html xmlns="http://www.w3.org/1999/xhtml">
          
          <head>
              <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
              <meta name="viewport" content="width=device-width, initial-scale=1.0" />
              <style type="text/css">
              </style>
          </head>
          
          <body>
              <h3>Estimad@ {nombre_dest} &#128075;,</h3>
              <p>Junto con saludar, informamos a usted que ha sido inscrito por la empresa <b>{razon_social}</b> a una instancia OnLine de <b>Capacitación</b> &#x1F469;&#x200D;&#x1F3EB;
              
              <p>Link de conexión &#128073; https://meet.google.com/rtx-kypb-kxu </p>
              <br>
              <p>Cabe mencionar que la asistencia es de carácter obligatorio y por lo tanto usted debe de asistir (Conectarse) para dar cumplimiento a lo establecido por su empresa, según se indica a continuación:</p>
              <ul>
                  <li>&#128198; Fecha: {cita$fecha_preparacion}</li>
                  <li>&#8986; Hora: {cita$horario}</li>
              </ul>
              <br>
              <p>(LEER INSTRUCCIONES)</p>
              <ul>
                  <li>Conectar desde dispositivo personal (&#128187; Computador o &#128241; Celular).</li>
                  <li>Conexión estable a internet (Obligatorio).</li>
                  <li>&#128247; Cámara y &#127908; Micrófono (Obligatorio).</li>
                  <li>&#127911; Audífonos (Obligatorio).</li>
                  <li>Contar con cuaderno y lápiz, para tomar apuntes &#9997;</li>
                  <li>Debe estar en un lugar tranquilo, sin ruidos ambientales, ni personas anexas (Obligatorio).</li>
                  <li>En caso de encontrarse en faena debe informar a su jefatura, para que le den libre en el horario y le dispongan de un lugar adecuado para la capacitación.</li>
              </ul>
              <br>
              <p>(CODIGO DE CONDUCTA)</p>
              <p>Al unirse a la sesión, usted está de acuerdo y acepta dar cumplimiento con lo siguiente:</p>
              <ul>
                  <li><u>Relacionarse de manera respetuosa con Coaches y el resto de los participantes.</u></li>
                  <li><u>No está permitido realizar grabaciones de video y/o audio durante las sesiones. La ley prohíbe filmar, fotografiar o grabar hechos o conversaciones de personas, sin consentimiento (código 161 A del Código Penal chileno que protege la privacidad - entendiendo como parte de ella la intimidad - de las personas)</u></li>
              </ul>
              <p style="color:red;"><b>Le notificamos que en caso de usted no respetar el código de conducta, la consultora recurrirá a realizar las denuncias correspondientes a su empresa y/o legales en caso de ser necesario &#128680;.</b></p>
              <br>
              <p>(CONSULTAS)</p>
              <p>En caso de dudas y/o consultas, favor comunicarse con:</p>
              <b>{solicitante}</b>
              <p style="margin:0">{cargo_solicitante}</p>
              <p style="margin:0">{razon_social}</p>
              <p style="margin:0">Teléfono: {telefono_solicitante}</p>
              <p style="margin:0">Correo: {email_solicitante}</p>
              <br>
              <p>Saludos cordiales / Best regards</p>
              <h4>Equipo MERC Training<br>
              MERC Consultora SpA</h4><br>
              <img src="https://www.mercconsultora.cl/img/logo6.png" width=180>
              <p style="color:#0086D8;"><b>“Aliados estratégicos en Consultoría de Gestión y Servicios Profesionales de Recursos Humanos.”</b></p>
          </body>
          
          </html>')
          )
  
  ## Override content type.
  body[["headers"]][["Content-Type"]] <- "text/html; charset=UTF-8"
  
  if (body$text == '') {
    email_result$status_code <- "000"
  } else {
    email_result <- sendmail(from = paste0("<", env$EMAIL_USERNAME, ">"),
                             to = paste0("<",dest,">"),
                             cc = paste0("<",emails_cc,">"),
                             bcc = paste0("<",emails_bcc,">"),
                             subject = subject,#bodyWithAttachment,
                             msg = list(body),
                             engine = "curl",
                             engineopts = list(username = env$EMAIL_USERNAME, password = env$EMAIL_PWD),
                             control=list(smtpServer= env$EMAIL_SMTP))
  }
  
  save_email(
    prep_id = prep_id,
    cita = cita, 
    email_participante = dest, 
    nombre_participante = nombre_dest, 
    rut_participante = rut, 
    email_from = emails_bcc,
    emp_razon_social = razon_social, 
    solicitante_nombre = solicitante, 
    solicitante_cargo = cargo_solicitante, 
    solicitante_telefono = telefono_solicitante, 
    solicitante_email = email_solicitante,
    email_body = body$text,
    status_code = email_result$status_code,
    razon = razon
  )
  
  return(email_result$status_code)
}

envio_email_solicitante <- function(cita, dest, nombre_dest, emails_cc = NULL, nombre_participante){

  subject <- "MERC: Confirmacion registro capacitacion"
  body <- mime_part(glue('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
          Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
          <html xmlns="http://www.w3.org/1999/xhtml">
          
          <head>
              <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
              <meta name="viewport" content="width=device-width, initial-scale=1.0" />
              <style type="text/css">
              </style>
          </head>
          
          <body>
              <h3>Estimado(a) {nombre_dest},</h3>
              <p>Junto con saludar, confirmamos que se ha recibido su solicitud de Capacitación para el participante {nombre_participante} y se ha enviado un correo indicando las coordinadas de conexión. </p>
              <br>
              <ul>
                  <li>Fecha: {cita$fecha_preparacion}</li>
                  <li>Hora: {cita$horario}</li>
              </ul>
              <br>
              <p>Cordialmente, Equipo MERC Consultora.</p>
          </body>
          
          </html>')
  )
  
  ## Override content type.
  body[["headers"]][["Content-Type"]] <- "text/html; charset=UTF-8"
  
  if (is.null(emails_cc)) {
    email_result <- sendmail(from = paste0("<", env$EMAIL_USERNAME, ">"),
                             to = paste0("<",dest,">"),
                             subject,#bodyWithAttachment,
                             msg = body,
                             engine = "curl",
                             engineopts = list(username = env$EMAIL_USERNAME, password = env$EMAIL_PWD),
                             control=list(smtpServer= env$EMAIL_SMTP))
    
  }else{
    email_result <- sendmail(from = paste0("<", env$EMAIL_USERNAME, ">"),
                             to = paste0("<",dest,">"),
                             cc = emails_cc,#paste0("<",emails_cc,">"),
                             subject,#bodyWithAttachment,
                             msg = body,
                             engine = "curl",
                             engineopts = list(username = env$EMAIL_USERNAME, password = env$EMAIL_PWD),
                             control=list(smtpServer= env$EMAIL_SMTP))
    
  }
  
  return(email_result$status_code)
}

# envio_email_pagos <- function(ep, dest, emails_cc, filename, asunto){
#   
#   subject <- asunto
#   
#   body <- mime_part(glue('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
#           Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
#           <html xmlns="http://www.w3.org/1999/xhtml">
#           
#           <head>
#               <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
#               <meta name="viewport" content="width=device-width, initial-scale=1.0" />
#               <style type="text/css">
#               </style>
#           </head>
#           
#           <body>
#               <h3>Estimado cliente &#128075;,</h3>
#               <p>Junto con saludar, solicitamos <b>Emitir Orden de Compra</b> correspondiente al <b>Estado de Pago</b>, según detalle en documento adjunto y resumen a continuación:
#               <br>
#               <br>
#               
#               <table style="border-color:#999999;border-width:2px;">
#                   <tbody>
#                       <tr>
#                           <td style="background-color:#0067c3;border-color:#999999;text-align:center;" colspan="2"><span style="color:#FFFFFF;"><span lang="es" dir="ltr"><strong>Resumen Servicio</strong></span></span></td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[1,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[1,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[2,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[2,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[3,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[3,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>Descripción:</strong></span></td>
#                           <td style="border-color:#999999;">{ep[4,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[5,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[5,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[6,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[6,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[7,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[7,]$valor}</td>
#                       </tr>
#                       <tr>
#                           <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[8,]$titulo}</strong></span></td>
#                           <td style="border-color:#999999;">{ep[8,]$valor}</td>
#                       </tr>
#                   </tbody>
#               </table>
#               <br>
#               <p>Saludos cordiales / Serdechne Vitannya / Best regards</p>
#               <h4>Luz Muñoz.<br>
#               Encargada de Administración & Finanzas<br>
#               MERC Consultora Limitada</h4><br>
#               <img src="https://www.mercconsultora.cl/img/logo6.png" width=180>
#               <p style="color:#0086D8;"><b>“Aliados estratégicos en Consultoría de Gestión y Servicios Profesionales de Recursos Humanos.”</b></p>
#               
#               <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
#                   <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Oficina MERC Antofagasta: </span></strong>
#                   <span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Baquedano #50, Oficina 709, Antofagasta &ndash; Chile.</span>
#               </p>
#               <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
#                   <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Tel.: </span></strong>
#                   <strong><span style="color: rgb(0, 121, 179); font-size:12px; font-family: Arial, sans-serif;">+56 9 57997112</span></strong>
#               </p>
#               <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
#                   <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Correo:</span></strong>
#                   <span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">&nbsp;</span>
#                   <u><span style="color: rgb(0, 121, 179); font-size:12px; font-family: Arial, sans-serif;">
#                       <a href="mailto:luz.mu%C3%B1oz@merconsultora.cl" target="_blank" style="color: rgb(17, 85, 204);">
#                           luz.muñoz@merconsultora.cl
#                       </a>
#                   </span></u>
#               </p>
#               <p style="text-align: start;color: rgb(34, 34, 34);background-color: rgb(255, 255, 255);font-size: small;font-family: Arial, Helvetica, sans-serif;"><a href="https://www.linkedin.com/in/mercconsultora/" target="_blank" style="color: rgb(17, 85, 204);"><img border="0" width="30" height="30" id="m_2549627942769778071Imagen_x0020_6" src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"></a></p>
#           </body>
#           
#           </html>')
#   )
#   
#   ## Override content type.
#   body[["headers"]][["Content-Type"]] <- "text/html; charset=UTF-8"
#   
#   attachmentObject <- mime_part(x = filename, name = filename)
#   
#   if (body$text == '') {
#     email_result$status_code <- "000"
#   } else {
#     args <- list(
#       from = paste0("<", env$EMAIL_PAGOS_USER, ">"),
#       to = paste0("<",str_trim(strsplit(dest, ",", fixed = TRUE)[[1]]),">"),
#       # cc = paste0("<",str_trim(strsplit(emails_cc, ",", fixed = TRUE)[[1]]),">"),
#       subject = subject,#bodyWithAttachment,
#       msg = list(body,attachmentObject),
#       engine = "curl",
#       engineopts = list(username = env$EMAIL_PAGOS_USER, password = env$EMAIL_PAGOS_PWD),
#       control=list(smtpServer= env$EMAIL_SMTP)
#     )
#     
#     if (emails_cc != '') {
#       args$cc = paste0("<",str_trim(strsplit(emails_cc, ",", fixed = TRUE)[[1]]),">")
#     }
#     
#     # email_result <- sendmail(from = paste0("<", env$EMAIL_PAGOS_USER, ">"),
#     #                          # to = paste0("<",dest,">"),
#     #                          to = paste0("<",str_trim(strsplit(dest, ",", fixed = TRUE)[[1]]),">"),
#     #                          # cc = paste0("<",emails_cc,">"),
#     #                          cc = paste0("<",str_trim(strsplit(emails_cc, ",", fixed = TRUE)[[1]]),">"),
#     #                          subject = subject,#bodyWithAttachment,
#     #                          msg = list(body,attachmentObject),
#     #                          engine = "curl",
#     #                          engineopts = list(username = env$EMAIL_PAGOS_USER, password = env$EMAIL_PAGOS_PWD),
#     #                          control=list(smtpServer= env$EMAIL_SMTP))
#     email_result <- do.call(sendmail, args)
#   }
#   
#   # save_email(
#   #   prep_id = prep_id,
#   #   cita = cita, 
#   #   email_participante = dest, 
#   #   nombre_participante = nombre_dest, 
#   #   rut_participante = rut, 
#   #   email_from = emails_bcc,
#   #   emp_razon_social = razon_social, 
#   #   solicitante_nombre = solicitante, 
#   #   solicitante_cargo = cargo_solicitante, 
#   #   solicitante_telefono = telefono_solicitante, 
#   #   solicitante_email = email_solicitante,
#   #   email_body = body$text,
#   #   status_code = email_result$status_code,
#   #   razon = razon
#   # )
#   
#   return(email_result$status_code)
# }

envio_email_pagos <- function(ep, dest, emails_cc, filename, asunto, body_text = NULL){
  
  subject <- asunto
  
  # --- ADJUSTMENT: Conditional Body Generation ---
  if (is.null(body_text) || body_text == "") {
    # Default HTML body for the first email (if body_text is not provided)
    body_content <- glue('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
          Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
          <html xmlns="http://www.w3.org/1999/xhtml">
          
          <head>
              <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
              <meta name="viewport" content="width=device-width, initial-scale=1.0" />
              <style type="text/css">
              </style>
          </head>
          
          <body>
              <h3>Estimado cliente &#128075;,</h3>
              <p>Junto con saludar, solicitamos <b>Emitir Orden de Compra</b> correspondiente al <b>Estado de Pago</b>, según detalle en documento adjunto y resumen a continuación:
              <br>
              
              <table style="border-color:#999999;border-width:2px;">
                  <tbody>
                      <tr>
                          <td style="background-color:#0067c3;border-color:#999999;text-align:center;" colspan="2"><span style="color:#FFFFFF;"><span lang="es" dir="ltr"><strong>Resumen Servicio</strong></span></span></td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[1,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[1,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[2,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[2,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[3,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[3,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>Descripción:</strong></span></td>
                          <td style="border-color:#999999;">{ep[4,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[5,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[5,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[6,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[6,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[7,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[7,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[8,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[8,]$valor}</td>
                      </tr>
                  </tbody>
              </table>
              <br>
              <p>Saludos cordiales / Serdechne Vitannya / Best regards</p>
              <h4>Luz Muñoz.<br>
              Encargada de Administración & Finanzas<br>
              MERC Consultora Limitada</h4><br>
              <img src="https://www.mercconsultora.cl/img/logo6.png" width=180>
              <p style="color:#0086D8;"><b>“Aliados estratégicos en Consultoría de Gestión y Servicios Profesionales de Recursos Humanos.”</b></p>
              
              <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
                  <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Oficina MERC Antofagasta: </span></strong>
                  <span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Baquedano #50, Oficina 709, Antofagasta &ndash; Chile.</span>
              </p>
              <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
                  <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Tel.: </span></strong>
                  <strong><span style="color: rgb(0, 121, 179); font-size:12px; font-family: Arial, sans-serif;">+56 9 57997112</span></strong>
              </p>
              <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
                  <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Correo:</span></strong>
                  <span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">&nbsp;</span>
                  <u><span style="color: rgb(0, 121, 179); font-size:12px; font-family: Arial, sans-serif;">
                      <a href="mailto:luz.mu%C3%B1oz@merconsultora.cl" target="_blank" style="color: rgb(17, 85, 204);">
                          luz.muñoz@merconsultora.cl
                      </a>
                  </span></u>
              </p>
              <p style="text-align: start;color: rgb(34, 34, 34);background-color: rgb(255, 255, 255);font-size: small;font-family: Arial, Helvetica, sans-serif;"><a href="https://www.linkedin.com/in/mercconsultora/" target="_blank" style="color: rgb(17, 85, 204);"><img border="0" width="30" height="30" id="m_2549627942769778071Imagen_x0020_6" src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"></a></p>
          </body>
          
          </html>')
    # Set Content-Type for HTML
    content_type <- "text/html; charset=UTF-8"
  } else {
    # Simple text body for the reminder email
    # Re-use the existing HTML footer/table structure but replace the primary body text
    body_content <- glue('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
          Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
          <html xmlns="http://www.w3.org/1999/xhtml">
          
          <head>
              <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
              <meta name="viewport" content="width=device-width, initial-scale=1.0" />
              <style type="text/css">
              </style>
          </head>
          
          <body>
              <h3>Estimado cliente &#128075;,</h3>
              <p>{body_text}</p> 
              <br>
              
              <table style="border-color:#999999;border-width:2px;">
                  <tbody>
                      <tr>
                          <td style="background-color:#0067c3;border-color:#999999;text-align:center;" colspan="2"><span style="color:#FFFFFF;"><span lang="es" dir="ltr"><strong>Resumen Servicio</strong></span></span></td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[1,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[1,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[2,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[2,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[3,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[3,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>Descripción:</strong></span></td>
                          <td style="border-color:#999999;">{ep[4,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[5,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[5,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[6,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[6,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[7,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[7,]$valor}</td>
                      </tr>
                      <tr>
                          <td style="background-color:#E6E6E6;border-color:#999999;"><span lang="es" dir="ltr"><strong>{ep[8,]$titulo}</strong></span></td>
                          <td style="border-color:#999999;">{ep[8,]$valor}</td>
                      </tr>
                  </tbody>
              </table>
              <br>
              <p>Saludos cordiales / Serdechne Vitannya / Best regards</p>
              <h4>Luz Muñoz.<br>
              Encargada de Administración & Finanzas<br>
              MERC Consultora Limitada</h4><br>
              <img src="https://www.mercconsultora.cl/img/logo6.png" width=180>
              <p style="color:#0086D8;"><b>“Aliados estratégicos en Consultoría de Gestión y Servicios Profesionales de Recursos Humanos.”</b></p>
              
              <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
                  <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Oficina MERC Antofagasta: </span></strong>
                  <span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Baquedano #50, Oficina 709, Antofagasta &ndash; Chile.</span>
              </p>
              <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
                  <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Tel.: </span></strong>
                  <strong><span style="color: rgb(0, 121, 179); font-size:12px; font-family: Arial, sans-serif;">+56 9 57997112</span></strong>
              </p>
              <p style="line-height: 0.2; text-align: start; color: rgb(34, 34, 34); background-color: rgb(255, 255, 255); font-size: small; font-family: Arial, Helvetica, sans-serif;">
                  <strong><span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">Correo:</span></strong>
                  <span style="color: rgb(89, 89, 89); font-size:12px; font-family: Arial, sans-serif;">&nbsp;</span>
                  <u><span style="color: rgb(0, 121, 179); font-size:12px; font-family: Arial, sans-serif;">
                      <a href="mailto:luz.mu%C3%B1oz@merconsultora.cl" target="_blank" style="color: rgb(17, 85, 204);">
                          luz.muñoz@merconsultora.cl
                      </a>
                  </span></u>
              </p>
              <p style="text-align: start;color: rgb(34, 34, 34);background-color: rgb(255, 255, 255);font-size: small;font-family: Arial, Helvetica, sans-serif;"><a href="https://www.linkedin.com/in/mercconsultora/" target="_blank" style="color: rgb(17, 85, 204);"><img border="0" width="30" height="30" id="m_2549627942769778071Imagen_x0020_6" src="https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"></a></p>
          </body>
          
          </html>')
    # Set Content-Type for HTML
    content_type <- "text/html; charset=UTF-8"
  }
  
  # Create the MIME part using the generated content
  body <- mime_part(body_content)
  
  ## Override content type.
  body[["headers"]][["Content-Type"]] <- content_type # Use the determined content type
  # ----------------------------------------------------
  
  attachmentObject <- mime_part(x = filename, name = filename)
  
  # The check for body$text == '' is now a bit complicated since we are using glue
  # A safer check for success would be at the end, but we'll stick to the original structure for now.
  # Assuming body_content is never empty due to the glue structure.
  
  # if (body$text == '') {
  #   email_result$status_code <- "000"
  # } else { 
  args <- list(
    from = paste0("<", env$EMAIL_PAGOS_USER, ">"),
    to = paste0("<",str_trim(strsplit(dest, ",", fixed = TRUE)[[1]]),">"),
    subject = subject,
    msg = list(body, attachmentObject),
    engine = "curl",
    engineopts = list(username = env$EMAIL_PAGOS_USER, password = env$EMAIL_PAGOS_PWD),
    control=list(smtpServer= env$EMAIL_SMTP)
  )
  
  if (emails_cc != '') {
    args$cc = paste0("<",str_trim(strsplit(emails_cc, ",", fixed = TRUE)[[1]]),">")
  }
  
  email_result <- do.call(sendmail, args)
  # }
  
  return(email_result$status_code)
}
