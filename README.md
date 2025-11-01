# Computing

Este repositorio reúne distintos proyectos y ejercicios desarrollados para practicar tecnologías web, análisis de datos y machine learning. Además se incluyen los recursos necesarios para ejecutar una pila de ejemplo formada por un frontend estático, un backend en FastAPI y una base de datos PostgreSQL.

## 📦 Instalación

1. **Requisitos previos**
   - Docker y Docker Compose Plugin (`docker compose`).
   - Bash (para ejecutar los scripts de automatización).

2. **Clonar el repositorio**
   ```bash
   git clone <url-del-repositorio>
   cd Computing
   ```

3. **Configurar variables de entorno**
   - Copia el archivo `.env.example` y renómbralo a `.env`.
   - Ajusta las credenciales de base de datos y las ventanas horarias de notificaciones según tus necesidades.
   ```bash
   cp .env.example .env
   ```

## ▶️ Uso

Con los requisitos previos instalados y el archivo `.env` configurado puedes levantar los servicios con Docker Compose:

```bash
docker compose up --build
```

Los servicios expuestos son:

| Servicio   | Puerto local | Descripción |
|------------|--------------|-------------|
| Frontend   | `http://localhost:8080` | Sitio estático contenido en `Desarrollo web/` servido con Nginx. |
| Backend    | `http://localhost:8000` | API FastAPI con endpoints de salud, perfil nutricional y consulta de horarios de notificación. |
| Base de datos | `localhost:5432` | Instancia PostgreSQL con volúmenes persistentes. |

Para detener los servicios ejecuta:

```bash
docker compose down
```

### Variables de entorno relevantes

| Variable | Descripción |
|----------|-------------|
| `POSTGRES_DB`, `POSTGRES_USER`, `POSTGRES_PASSWORD`, `POSTGRES_HOST`, `POSTGRES_PORT` | Configuración de la base de datos PostgreSQL. |
| `NOTIFICATION_MORNING_WINDOW`, `NOTIFICATION_EVENING_WINDOW` | Ventanas horarias (formato `HH:MM-HH:MM`) utilizadas por el backend para programar notificaciones. |
| `NOTIFICATION_TIMEZONE` | Zona horaria que se empleará al interpretar las ventanas configuradas. |

El backend expone el endpoint `GET /notifications/schedule` para consultar los valores actuales de estas variables.

### Perfil nutricional

El servicio de FastAPI incluye un recurso `/profile/{profile_id}` que permite persistir métricas y preferencias del usuario:

* `GET /profile/{profile_id}` devuelve (y crea si no existe) el perfil con peso, altura, objetivos y restricciones alimentarias.
* `PUT /profile/{profile_id}` actualiza el perfil con los datos enviados en el cuerpo de la petición.

Por defecto la aplicación utiliza SQLite (`backend/app/data/app.db`), aunque se puede establecer la variable de entorno `PROFILE_DATABASE_URL` para apuntar a otra base de datos compatible con SQLAlchemy.

## 🚀 CI/CD

El flujo definido en `.github/workflows/deploy.yml` construye las imágenes, valida las variables de horario de notificación y compila el backend. Cuando se hace push a la rama `main`, el job de despliegue ejecuta `scripts/deploy.sh`, que automatiza la construcción y el arranque de los servicios con Docker Compose.

Si deseas realizar un despliegue manual, ejecuta:

```bash
./scripts/deploy.sh
```

Asegúrate de tener un archivo `.env` válido antes de lanzar el script.

## 🤝 Contribuir

1. Crea un fork del repositorio y una rama con un nombre descriptivo.
2. Realiza cambios siguiendo las buenas prácticas del lenguaje o herramienta utilizada.
3. Ejecuta `docker compose build` para confirmar que las imágenes se construyen correctamente.
4. Abre un Pull Request describiendo claramente los cambios e incluye los resultados de las pruebas relevantes.

Toda contribución es bienvenida. ¡Gracias por colaborar!
