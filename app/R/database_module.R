get_id_empresa <- function(email_usuario){
  id <- tbl(pool, "usuarios") %>%
    filter(email == email_usuario)
}

check_disabled_user <- function(email_usuario){
  # sql <- glue::glue_sql("select bloqueado from clientes where email_contacto = {email}", .con = pool)
  # print(paste("db:", dbExecute(pool, sql)))
  # return(dbGetQuery(pool, sql) %>% pull(bloqueado))
  
  dbExecute(pool, 'SET character set "utf8"')
  flag <- tbl(pool, "usuarios") %>% 
    filter(email == email_usuario) %>% 
    select(email, id_empresa, rol, nombre, apellidos, cargo, telefono, bloqueado) %>% 
    rename(user_blocked = bloqueado) %>% 
    inner_join(tbl(pool, "clientes"), by = c("id_empresa")) %>% 
    select(id_empresa, razon_social, bloqueado, rol, email, nombre, apellidos, cargo, telefono, user_blocked) %>% 
    dplyr::collect() #%>% 
    #pull(bloqueado)
  return(flag)
}

get_coach_id <- function(email_usuario){
  id <- tbl(pool, "coach") %>% 
    filter(email == email_usuario) %>% 
    select(id) %>% 
    dplyr::collect() %>% 
    dplyr::pull()
  
  return(id)
}

get_email_estado_pago <- function(email_cliente){
  email <- tbl(pool, "clientes") %>% 
    filter(email_contacto == email_cliente) %>%
    inner_join(tbl(pool, "info_estado_pagos"), by = c("id_empresa")) %>% 
    select(email_para_envios) %>% 
    collect() %>%
    pull(email_para_envios)
  return(email)
}

get_estado_pago_info <- function(id_emp){
  info <- tbl(pool, "info_estado_pagos") %>%
    filter(id_empresa == id_emp) %>% 
    collect()
  
  result <- info
  print(result)
  return(result)
}

get_users <- function(id_emp){
  if (is.null(id_emp)) {
    users <- data.frame(#id = numeric(),
                        nombre = character(), 
                        apellidos = character(), 
                        rut = character(),
                        telefono = character(),
                        email = character(),
                        cargo = character(),
                        rol = character(),
                        bloqueado = integer(),
                        inactivo = character(),
                        ultimo_login = character())
  }else{
    dbExecute(pool, 'SET character set "utf8"')
    users <- tbl(pool, "usuarios") %>% 
      filter(id_empresa == id_emp) %>% 
      select(nombre, apellidos, rut, telefono, email, cargo, rol, bloqueado, inactivo, ultimo_login) %>% 
      mutate(
        bloqueado = if_else(bloqueado, "Bloqueado", "Desbloqueado"),
        inactivo = if_else(inactivo == 1, "Inactivo", "Activo")
        # ultimo_login = if_else(
        #   is.na(ultimo_login), 
        #   "Nunca", 
        #   format(as.POSIXct(ultimo_login), "%d-%m-%Y %H:%M")
        # )
      ) %>% 
      collect()
  }
  
  return(users)
}

get_user_info <- function(user_email){
  dbExecute(pool, 'SET character set "utf8"')
  user_info <- tbl(pool, "usuarios") %>% 
    filter(email == user_email) %>% 
    select(nombre, apellidos) %>%
    collect()
  
  return(user_info)
}

get_cc <- function(id_emp){
  if (is.null(id_emp)) {
    cc <- data.frame(id = numeric(),
                     nombre = character())
  }else{
    dbExecute(pool, 'SET character set "utf8"')
    cc <- tbl(pool, "centro_de_costos") %>% 
      filter(id_empresa == id_emp) %>% 
      select(-id_empresa) %>%
      collect()
  }
  return(cc)
}

get_centro_de_costos <- function(id_emp){
  dbExecute(pool, 'SET character set "utf8"')
  cc <- tbl(pool, "centro_de_costos") %>% 
          filter(id_empresa == id_emp) %>% 
          select(id, nombre) %>%
          collect() %>%
          pull()
  # print(cc)
  return(cc)
}

get_centro_de_costos_por_participante <- function(id_participante){
  dbExecute(pool, 'SET character set "utf8"')
  cc <- dplyr::tbl(pool, "participantes") %>% 
    dplyr::filter(id == id_participante) %>% 
    dplyr::select(id_empresa) %>% 
    dplyr::inner_join(
      dplyr::tbl(pool, "centro_de_costos"), by = "id_empresa"
    ) %>% 
    dplyr::select(nombre) %>% 
    collect() %>%
    tibble::deframe()
  
  return(cc)
}

get_coaches <- function(){
  dbExecute(pool, 'SET character set "utf8"')
  coaches <- tbl(pool, "coach") %>% 
    filter(activo == TRUE) %>% 
    mutate(nombre = paste(nombres_coach, apellidos_coach)) %>% 
    select(nombre, id) %>% 
    collect() %>%
    tibble::deframe() %>%
    as.list()
  #print(coaches)
  return(coaches)
}

get_tarifas <- function(id_emp){
  tarifa <- tbl(pool, "tarifas") %>% 
    filter(id_empresa == id_emp) %>% 
    select(tarifa_normal, tarifa_urgente, unidad_UF) %>% 
    collect()
  return(tarifa)
}

get_lista_emails <- function(id_emp){
  emails <- tbl(pool, "inscripcion_notificaciones") %>%
    filter(id_empresa == id_emp) %>%
    select(lista_emails) %>% 
    collect() %>% 
    pull()
  
  return(emails)
}

get_link_factura <- function(id_emp){
  url <- tbl(pool, "link_facturas") %>% 
    filter(id_empresa == id_emp) %>%
    select(url) %>% 
    collect() %>% 
    pull()
  
  return(url)
}

get_empresas <- function(rol_usuario, email_usuario){
  
  dbExecute(pool, 'SET character set "utf8"')
  if (rol_usuario %in% c('admin','coach','asistente','coordinador', 'administrativo')) {
    listado <- tbl(pool, "clientes") %>% 
      select(razon_social, id_empresa) %>% 
      collect() %>% 
      add_row(razon_social = 'Todos', id_empresa = 0, .before = 1) %>%
      # add_row(razon_social = c(' ','Todos'), id_empresa = c(-1, 0), .before = 1) %>%
      tibble::deframe()
  }else{
    listado <- tbl(pool, "usuarios") %>% 
      filter(email == email_usuario) %>% 
      select(email, id_empresa) %>% 
      inner_join(tbl(pool, "clientes"), by = "id_empresa") %>% 
      select(razon_social, id_empresa) %>% 
      collect() %>% 
      tibble::deframe()
  }

  return(listado)
}

get_valores_unitarios <- function(pid_emp){
  listado <- tbl(pool, "tarifas") %>% 
    select(id_empresa, tarifa_normal, unidad_UF) %>% 
    filter(id_empresa == pid_emp) %>%
    collect()
  
  if (pid_emp == -1) {
    listado <- listado %>% 
      add_row(id_empresa = 0, tarifa_normal = NA, unidad_UF = 1, .before = 1)
      
  }
  
  listado <- listado %>% 
    select(tarifa_normal, unidad_UF)
  
  return(listado)
}

add_new_user <- function(user, id_emp){
  dbExecute(pool, 'SET character set "utf8"')
  query <- sqlAppendTable(pool, "usuarios", user, row.names = FALSE)
  result <- dbExecute(pool, query)
  return(result)
}

add_new_cc <- function(cc, id_emp){
  dbExecute(pool, 'SET character set "utf8"')
  query <- sqlAppendTable(pool, "centro_de_costos", cc, row.names = FALSE)
  result <- dbExecute(pool, query)
  return(result)
}

add_new_obs <- function(obs){
  dbExecute(pool, 'SET character set "utf8"')
  query <- sqlAppendTable(pool, "observaciones", obs, row.names = FALSE)
  result <- dbExecute(pool, query)
  return(result)
}

editar_obs <- function(obs){
  sqlq <- glue::glue_sql("UPDATE observaciones set nombre = {obs$nombre}, descripcion = {obs$descripcion}, tipo = {obs$tipo}, active = {obs$active} WHERE id = {obs$id}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET character set "utf8"')
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

desactivar_obs <- function(obs_id){
  sqlq <- glue::glue_sql("UPDATE observaciones set active = false WHERE id = {obs_id}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

save_estado_de_pago_info <- function(id_emp, inicio_servicio, cierre_servicio, email_estado_pago, email_estado_pago_cc, tiempo_espera){

  sqlq <- glue::glue_sql("UPDATE info_estado_pagos set fecha_inicio_servicio = {inicio_servicio}, fecha_cierre_servicio = {cierre_servicio}, tiempo_espera = {tiempo_espera}, email_para_envios = {email_estado_pago}, email_para_envios_cc = {email_estado_pago_cc} WHERE id_empresa = {id_emp}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

save_tarifa <- function(id_emp, tarifa_normal, tarifa_urgente, unidad_moneda){
  tarifa_normal <- as.numeric(sub(",", ".", tarifa_normal, fixed = TRUE))
  tarifa_urgente <- as.numeric(sub(",", ".", tarifa_urgente, fixed = TRUE))
  sqlq <- glue::glue_sql("UPDATE tarifas set tarifa_normal = {tarifa_normal}, tarifa_urgente = {tarifa_urgente}, unidad_UF = {unidad_moneda} WHERE id_empresa = {id_emp}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

save_inscripciones_notificacion_por_email <- function(id_emp, emails){
  sqlq <- glue::glue_sql("UPDATE inscripcion_notificaciones set lista_emails = {emails} WHERE id_empresa = {id_emp}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

save_link_factura <- function(id_emp, link){
  sqlq <- glue::glue_sql("UPDATE link_facturas set url = {link} WHERE id_empresa = {id_emp}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

save_archivo_resultados <- function(resultados){
  print("dentro de save_archivo_resultados")
  
  clientes <- tbl(pool, 'clientes') %>% 
    select(id_empresa, rut_cliente) %>% 
    collect()
  print(clientes)
  resultados  <- resultados %>% 
    left_join(
      clientes, by = c('rut_empresa'='rut_cliente')
    ) %>% 
    filter(!is.na(id_empresa)) %>% 
    #select(-rut_empresa, -rut_cliente) %>% 
    relocate(id_empresa)
  print(head(resultados))
    #distinct(rut_empresa) %>% add_row(rut_empresa = '13123123123') %>%  pull()
  id_empresas <- resultados %>% distinct(id_empresa) %>% pull()
  # dbExecute(pool, 'TRUNCATE `capacitacion3d`.`resultados_planilla`')
  
  sqlq <- glue::glue_sql("delete from {`db`}.`resultados_planilla` where id_empresa in ({id_empresas*})", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
  resultados <- resultados %>% select(-rut_empresa)
  dbExecute(pool, 'SET character set "utf8"')
  query <- sqlAppendTable(pool, "resultados_planilla", resultados, row.names = FALSE)
  dbExecute(pool, query)
  rm(resultados)
}

# save_archivo_resultados <- function(resultados){
#   print("dentro de save_archivo_resultados")
#   id_empresas <- resultados %>% distinct(rut_empresa) %>% pull()
#   # dbExecute(pool, 'TRUNCATE `capacitacion3d`.`resultados_planilla`')
#   sql <- glue::glue_sql("delete from `capacitacion3d`.`resultados_planilla` where id_empresa in ({id_empresas})")
#   print(sql)
#   dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
#   dbExecute(pool, sqlq)
#   dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
#   resultados <- resultados %>% select(-rut_empresa)
#   dbExecute(pool, 'SET character set "utf8"')
#   #query <- sqlAppendTable(pool, "resultados_planilla", resultados, row.names = FALSE)
#   dbExecute(pool, query)
# }

update_solicitud_especial <- function(rut, fecha, horario){
  sql1 <- glue::glue_sql("select id from participantes where fecha_solicitud = {as.character(today(tzone = 'Chile/Continental'))} and rut = {rut}", .con = pool)
  id_participante <- dbGetQuery(pool, sql1) %>% pull()
  
  sqlq <- glue::glue_sql("UPDATE monitor_preparaciones set fecha_preparacion = {fecha}, horario = {horario} WHERE id_participante = {id_participante}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

update_training_blocks <- function(pfecha, phorario, pval){
  current <- tbl(pool, "disponibilidad_preparaciones") %>% 
    dplyr::filter(fecha == pfecha) %>% 
    collect()
  
  columna <- case_when(
    phorario == '09:00 am' ~ '9am',
    phorario == '12:00 pm' ~ '12pm',
    phorario == '03:00 pm' ~ '3pm',
    phorario == '06:00 pm' ~ '6pm',
    .default = NA
  )
  
  valor <- case_when(
    phorario == '09:00 am' ~ current$`9am`,
    phorario == '12:00 pm' ~ current$`12pm`,
    phorario == '03:00 pm' ~ current$`3pm`,
    phorario == '06:00 pm' ~ current$`6pm`,
    .default = NA
  )
  
  sql <- glue::glue_sql(
    "UPDATE disponibilidad_preparaciones set {`columna`} = {valor}+{pval} WHERE fecha = {pfecha}"
  , .con = pool)
  
  if (!is.na(columna)) {
    dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
    dbExecute(pool, sql)
    dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
  }
}

obtener_fecha_hora_cita <- function(pid_emp, prut = NA, pfecha = NA, pid_prep = NA, pid_participante = NA){
  if (is.na(pid_prep)) {
    if (is.na(pid_participante)) {
      cita <- tbl(pool, "preparaciones_view") %>% 
        filter(id_empresa == pid_emp & rut == prut & fecha_solicitud == pfecha) %>% 
        select(fecha_preparacion, horario) %>% 
        collect() %>% 
        as.list()
    }else{
      cita <- tbl(pool, "monitor_preparaciones") %>% 
        filter(id_participante == pid_participante) %>% 
        select(fecha_preparacion, horario) %>% 
        collect() %>% 
        as.list()
    }
  } else {
    cita <- tbl(pool, "preparaciones_view") %>% 
      filter(id_empresa == pid_emp & id_preparacion == pid_prep) %>% 
      select(fecha_preparacion, horario) %>% 
      collect() %>% 
      as.list()
  }
  
  return(cita)
}

generar_estado_de_pago <- function(pid_emp, pfecha_servicio, pfecha_gen, potra_tarifa, pnueva_tarifa, pvalor_cambio, pfecha_uf, pusuario){
  es_posible_generar_ep <- 0
  
  where_filters <- glue::glue_sql("id_empresa = {pid_emp} and date_format(cast(fecha_preparacion as date), '%m-%Y') = {pfecha_servicio}", .con = pool)
  
  if (pid_emp == 0) {
    where_filters <- glue::glue_sql("date_format(cast(fecha_preparacion as date), '%m-%Y') = {pfecha_servicio}", .con = pool)
  }
    
  sql <- glue::glue_sql("SELECT count(*) as result from preparaciones_view pv where ({where_filters})", .con = pool)
  
  es_posible_generar_ep <- dbGetQuery(pool, sql) %>% pull()
  
  if (es_posible_generar_ep > 0) {
    data <- data.frame(
      id_empresa = pid_emp,
      fecha_servicio = pfecha_servicio,
      fecha_generacion = pfecha_gen,
      usar_otra_tarifa = potra_tarifa,
      nueva_tarifa = pnueva_tarifa,
      valor_cambio = pvalor_cambio,
      fecha_uf = pfecha_uf,
      generado_por = pusuario
    )
    print(data)
    dbExecute(pool, 'SET character set "utf8"')
    quary <- sqlAppendTable(pool, "generacion_estado_de_pago", data, row.names = FALSE)
    dbExecute(pool, quary)
  }
  
  return(es_posible_generar_ep)
}

get_estados <- function(pcontexto){
  
  dbExecute(pool, 'SET character set "utf8"')
  listado <- tbl(pool, "estados") %>% 
    filter(contexto == pcontexto) %>%
    select(nombre, id_nombre) %>% 
    collect() %>% 
    tibble::deframe()
  
  return(listado)
}

guardar_estados_ep <- function(pid_ep, pid_oc, pid_pago){
  sqlq <- glue::glue_sql("UPDATE estado_de_pago set estado_pago = {as.numeric(pid_pago)} WHERE id = {as.numeric(pid_ep)}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

get_notificaciones <- function(pid_emp){
  hoy <- today(tzone = "Chile/Continental")
  dbExecute(pool, 'SET character set "utf8"')
  listado <- tbl(pool, "notificaciones") %>% 
    filter((reach == 0 | reach == pid_emp) & caduca >= hoy) %>%
    collect()
}

get_solicitante_info <- function(pemail){
  dbExecute(pool, 'SET character set "utf8"')
  info <- tbl(pool, "usuarios") %>% 
    filter(email == pemail) %>% 
    select(nombre, apellidos, telefono, cargo, id_empresa) %>% 
    collect() %>%
    as.list()
  #print(coaches)
  return(info)
}

get_available_slots <- function(pfecha){
  disponibilidad <- tbl(pool, "disponibilidad_preparaciones") %>% 
    dplyr::filter(fecha == pfecha) %>%
    collect()
  #print(coaches)
  return(disponibilidad)
}

get_proyectos <- function(pid_empresa){
  dbExecute(pool, 'SET character set "utf8"')
  if (pid_empresa == 0) {
    listado <- tbl(pool, "estadisticas_view") %>% 
      mutate(proyecto_id = proyecto) %>% 
      select(proyecto, proyecto_id) %>% 
      distinct(proyecto, .keep_all = T) %>% 
      collect() %>% 
      mutate(proyecto = replace_na(proyecto, "No definido")) %>% 
      add_row(proyecto = 'Todos', proyecto_id = "Todos", .before = 1) %>% 
      tibble::deframe()
  }else{
    listado <- tbl(pool, "estadisticas_view") %>% 
      filter(id_empresa == pid_empresa) %>% 
      mutate(proyecto_id = proyecto) %>% 
      select(proyecto, proyecto_id) %>% 
      distinct(proyecto, .keep_all = T) %>% 
      collect() %>% 
      mutate(proyecto = replace_na(proyecto, "No definido")) %>% 
      add_row(proyecto = 'Todos', proyecto_id = "Todos", .before = 1) %>% 
      tibble::deframe()
  }
  
  return(listado)
}

get_info_actualizacion_resultados <- function(pid_empresa){
  dbExecute(pool, 'SET character set "utf8"')
  if (pid_empresa == 0) {
    info <- tbl(pool, "estadisticas_view") %>% 
      # select(updated_by, fecha_carga_datos) %>%
      # group_by(updated_by) %>%
      select(fecha_carga_datos) %>%
      group_by(.) %>%
      summarise(fecha = max(fecha_carga_datos)) %>%
      collect() %>% 
      mutate(update_by = NA)
  }else{
    info <- tbl(pool, "estadisticas_view") %>% 
      filter(id_empresa == pid_empresa) %>% 
      select(updated_by, fecha_carga_datos) %>%
      group_by(updated_by) %>%
      # select(fecha_carga_datos) %>%
      # group_by(.) %>%
      summarise(fecha = max(fecha_carga_datos)) %>%
      collect() 
  }
  
  return(info)
}

se_puede_editar <- function(pid_participante){
  estado <- tbl(pool, "monitor_preparaciones") %>% 
    filter(id_participante == pid_participante) %>% 
    select(estado) %>% 
    pull()
  
  return(!estado %in% c('capacitado', 'inasistencia', 'abandona', 'suspendida'))
}

get_prefacturas <- function(pid_empresa){
  message("getting prefacturas")
  prefacturas <- tbl(pool, "prefacturas_view") %>% 
    filter(id_empresa == pid_empresa & estado == 'OC pendiente') %>% 
    collect() %>% 
    mutate(retraso = round(as.numeric(difftime(lubridate::now(tzone = "Chile/Continental"), ymd_hms(fecha_generacion), units = "days"))))
  
  
  return(prefacturas)
}

log_user_login <- function(user_id, session_id){
  
}

get_psicologos <- function(){
  dbExecute(pool, 'SET character set "utf8"')
  coaches <- tbl(pool, "coach") %>% 
    filter(id != 1) %>% 
    mutate(nombre = paste(nombres_coach, apellidos_coach)) %>% 
    select(nombre, id) %>% 
    collect() %>% 
    add_row(nombre = "Todos", id = 0, .before = 1) %>%
    tibble::deframe()
}

save_email <- function(prep_id, cita, email_participante, nombre_participante, rut_participante, email_from, emp_razon_social, solicitante_nombre, solicitante_cargo, solicitante_telefono, solicitante_email, email_body, status_code, razon) {
  data <- data.frame( 
    timestamp = lubridate::now(tzone = "Chile/Continental"),
    prep_id = prep_id,
    cita_fecha = cita$fecha,
    cita_hora = cita$hora,
    email_participante = email_participante,
    nombre_participante = nombre_participante,
    rut_participante = rut_participante,
    email_from = email_from,
    emp_razon_social = emp_razon_social,
    solicitante_nombre = solicitante_nombre,
    solicitante_cargo = solicitante_cargo,
    solicitante_telefono = solicitante_telefono,
    solicitante_email = solicitante_email,
    email_body = email_body,
    status = status_code,
    razon = razon
  )
  
  dbExecute(pool, 'SET character set "utf8"')
  quary <- sqlAppendTable(pool, "email_audit", data, row.names = FALSE)
  dbExecute(pool, quary)
}

get_system_variable <- function(module, submodule, var_name){
  tbl(pool, "variables_configuracion_sistema") %>%
    filter(modulo == module & variable == var_name) %>% 
    select(valor_num) %>% 
    pull()
}

set_system_variable <- function(module, submodule, var_name, value){
  sqlq <- glue::glue_sql("UPDATE variables_configuracion_sistema set valor_num = {as.numeric(value)} WHERE modulo = {module} and variable = {var_name}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

get_info_ep <- function(id_emp){
  tbl(pool, "info_estado_pagos") %>%
    filter(id_empresa == id_emp) %>% 
    collect()
}

email_ep_enviado <- function(var_id_emp, var_mes_servicio, var_valor){
  sqlq <- glue::glue_sql("UPDATE estado_de_pago set num_email_enviados = {var_valor+1} WHERE id_empresa = {var_id_emp} and fecha_servicio = {var_mes_servicio}", .con = pool)
  print(sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

get_num_ep_emails <- function(var_id_emp, var_mes_servicio){
  tbl(pool, "estado_de_pago") %>%
    filter(id_empresa == var_id_emp & fecha_servicio == var_mes_servicio) %>% 
    select(num_email_enviados) %>% 
    pull() %>% 
    as.numeric()
}

#' Update user's last login timestamp
#' Called every time a user logs in
#' @param email_usuario User's email address
update_ultimo_login <- function(email_usuario) {
  sqlq <- glue::glue_sql(
    "UPDATE usuarios SET ultimo_login = NOW() WHERE email = {email_usuario}", 
    .con = pool
  )
  
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

#' Check if user is inactive (no login for more than 6 months)
#' @param email_usuario User's email address
#' @return TRUE if user should be considered inactive, FALSE otherwise
check_user_inactivity <- function(email_usuario) {
  result <- tbl(pool, "usuarios") %>%
    filter(email == email_usuario) %>%
    select(ultimo_login, inactivo) %>%
    collect()
  
  if (nrow(result) == 0) {
    return(list(is_inactive = FALSE, flag_inactivo = FALSE))
  }
  
  ultimo_login <- result$ultimo_login
  flag_inactivo <- as.logical(result$inactivo)
  
  # If user is already flagged as inactive, return TRUE
  if (flag_inactivo) {
    return(list(is_inactive = TRUE, flag_inactivo = TRUE))
  }
  
  # If ultimo_login is NULL (first login or legacy user), not inactive
  if (is.na(ultimo_login) || is.null(ultimo_login)) {
    return(list(is_inactive = FALSE, flag_inactivo = FALSE))
  }
  
  num_meses_inactividad <- as.numeric(get_system_variable('sistema', NULL, 'numero_de_meses_de_inactividad'))
  if (is.null(num_meses_inactividad) || length(num_meses_inactividad) == 0) {
    num_meses_inactividad <- 6
  }
  
  # Check if more than 6 months have passed since last login
  six_months_ago <- lubridate::now(tzone = "Chile/Continental") - months(num_meses_inactividad)
  is_inactive <- as.POSIXct(ultimo_login) < six_months_ago
  
  return(list(is_inactive = is_inactive, flag_inactivo = flag_inactivo))
}

#' Automatically deactivate user due to inactivity
#' @param email_usuario User's email address
deactivate_user_inactivity <- function(email_usuario) {
  sqlq <- glue::glue_sql(
    "UPDATE usuarios SET inactivo = 1 WHERE email = {email_usuario}", 
    .con = pool
  )
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

#' Reactivate user (clear inactivity flag)
#' @param user_id User's ID
reactivate_user <- function(user_id) {
  sqlq <- glue::glue_sql(
    "UPDATE usuarios SET inactivo = 0, ultimo_login = NOW() WHERE id = {user_id}", 
    .con = pool
  )
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

#' Update user inactivity status
#' @param user_id User's ID
#' @param inactivo Boolean indicating if user should be inactive
set_user_inactivity_status <- function(user_id, inactivo) {
  sqlq <- glue::glue_sql(
    "UPDATE usuarios SET inactivo = {as.numeric(inactivo)} WHERE id = {user_id}", 
    .con = pool
  )
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 0')
  dbExecute(pool, sqlq)
  dbExecute(pool, 'SET SQL_SAFE_UPDATES = 1')
}

#' Get user info including inactivity status
#' @param email_usuario User's email address
#' @return Data frame with user info including inactivo flag
get_user_full_info <- function(email_usuario) {
  dbExecute(pool, 'SET character set "utf8"')
  user_info <- tbl(pool, "usuarios") %>% 
    filter(email == email_usuario) %>% 
    select(id, nombre, apellidos, email, inactivo, ultimo_login, bloqueado) %>%
    collect()
  
  return(user_info)
}

#' Save generated certificate record to the database
#' @param folio unique certificate folio (e.g. MERC-CC-2025-0110-PAAM-001)
#' @param participante full name of the participant
#' @param rut participant's RUT
#' @param cargo participant's job title
#' @param empresa company name
#' @param contrato_proyecto contract or project name
#' @param tipo_actividad type of activity
#' @param fecha_realizacion date the training took place
#' @param duracion duration of the training
#' @param subdimensiones JSON array string of skills evaluated
#' @param alcance scope description
save_certificado <- function(folio, participante, rut, cargo, empresa,
                             contrato_proyecto, tipo_actividad, fecha_realizacion,
                             duracion, subdimensiones, alcance, id_preparacion) {
  data <- data.frame(
    folio              = folio,
    participante       = participante,
    rut                = rut,
    cargo              = cargo,
    empresa            = empresa,
    contrato_proyecto  = contrato_proyecto,
    tipo_actividad     = tipo_actividad,
    fecha_realizacion  = as.character(fecha_realizacion),
    duracion           = duracion,
    subdimensiones     = subdimensiones,
    alcance            = alcance,
    created_at         = lubridate::now(tzone = "Chile/Continental"),
    updated_at          = lubridate::now(tzone = "Chile/Continental"),
    id_preparacion     = id_preparacion,
    stringsAsFactors   = FALSE
  )
  
  dbExecute(pool, 'SET character set "utf8"')
  query <- sqlAppendTable(pool, "certificados", data, row.names = FALSE)
  dbExecute(pool, query)
}

#' Check if a certificate already exists for a given preparation ID
#' @param p_id_preparacion the preparation ID to check
#' @return TRUE if a certificate already exists, FALSE otherwise
certificado_exists <- function(p_id_preparacion) {
  result <- tbl(pool, "certificados") %>%
    filter(id_preparacion == p_id_preparacion) %>%
    summarise(n = n()) %>%
    collect() %>%
    pull(n)
  
  return(result > 0)
}

#' Get existing certificate folio for a preparation
#' @param id_preparacion The preparation ID
#' @return Data frame with folio or empty
get_certificado_folio <- function(id_preparacion) {
  tbl(pool, "certificados") %>%
    filter(id_preparacion == !!id_preparacion) %>%
    select(folio) %>%
    collect()
}