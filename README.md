# Computing

Este repositorio reúne distintos proyectos y ejercicios desarrollados para practicar tecnologías web, análisis de datos y machine learning. Además se incluyen los recursos necesarios para ejecutar una pila de ejemplo formada por un frontend estático y un backend en FastAPI que expone la configuración de ventanas horarias para el envío de notificaciones.

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
   - Define las variables `NOTIFICATION_MORNING_WINDOW`, `NOTIFICATION_EVENING_WINDOW` y `NOTIFICATION_TIMEZONE` para personalizar el horario de notificaciones. Cada ventana debe utilizar el formato `HH:MM-HH:MM`.

## ▶️ Uso

### Backend

Con Python 3.11 instalado, crea un entorno virtual y ejecuta:

```bash
pip install -r requirements.txt
uvicorn backend.app.main:app --reload
```

La API expondrá:

| Endpoint | Descripción |
|----------|-------------|
| `GET /health` | Devuelve `{ "status": "ok" }` para comprobar que el servicio está operativo. |
| `GET /notifications/schedule` | Devuelve las ventanas horarias activas y la zona horaria configurada. |

### Frontend
| Servicio   | Puerto local | Descripción |
|------------|--------------|-------------|
| Frontend   | `http://localhost:8080` | Sitio estático contenido en `Desarrollo web/` servido con Nginx. |
| Backend    | `http://localhost:8000` | API FastAPI con endpoints de salud, perfil nutricional y consulta de horarios de notificación. |
| Base de datos | `localhost:5432` | Instancia PostgreSQL con volúmenes persistentes. |

El directorio `Desarrollo web/` contiene el sitio estático. Puedes servirlo con cualquier servidor de archivos estáticos. Un ejemplo rápido utilizando `python -m http.server`:

```bash
cd "Desarrollo web"
python -m http.server 8080
```

### Nevera virtual

El directorio `backend/` incluye una API en FastAPI que implementa la lógica de la nevera virtual: inventario de alimentos, registro manual, interpretación de tickets mediante OCR y sugerencia de recetas.

```bash
pip install -r backend/requirements.txt
uvicorn app.main:app --reload --app-dir backend
```

Por defecto la API se expone en `http://localhost:8000/api`. Desde el navegador puedes abrir `Desarrollo web/virtual-fridge/index.html` (sirviéndolo con cualquier servidor estático) para gestionar la UI del inventario. Ajusta la variable global `APP_CONFIG.API_BASE` si publicas backend y frontend en hosts distintos.

> ℹ️ Para habilitar el escaneo de tickets instala el binario `tesseract-ocr` en el sistema donde se ejecute el backend.

### Variables de entorno relevantes

| Variable | Descripción |
|----------|-------------|
| `NOTIFICATION_MORNING_WINDOW`, `NOTIFICATION_EVENING_WINDOW` | Ventanas horarias (formato `HH:MM-HH:MM`) utilizadas por el backend para programar notificaciones. |
| `NOTIFICATION_TIMEZONE` | Zona horaria que se empleará al interpretar las ventanas configuradas. |

El backend expone el endpoint `GET /notifications/schedule` para consultar los valores actuales de estas variables.

## 🧪 Pruebas
### Perfil nutricional

El servicio de FastAPI incluye un recurso `/profile/{profile_id}` que permite persistir métricas y preferencias del usuario:

* `GET /profile/{profile_id}` devuelve (y crea si no existe) el perfil con peso, altura, objetivos y restricciones alimentarias.
* `PUT /profile/{profile_id}` actualiza el perfil con los datos enviados en el cuerpo de la petición.

Por defecto la aplicación utiliza SQLite (`backend/app/data/app.db`), aunque se puede establecer la variable de entorno `PROFILE_DATABASE_URL` para apuntar a otra base de datos compatible con SQLAlchemy.

## 🚀 CI/CD

El repositorio incluye tests unitarios y de extremo a extremo para el backend. Para ejecutarlos junto con el linting utiliza:

```bash
pip install -r requirements-dev.txt
ruff check .
pytest
```

El flujo de integración continua definido en `.github/workflows/ci.yml` ejecuta los mismos pasos en GitHub Actions.

### Pruebas manuales

Antes de publicar una versión revisa manualmente:

1. Cargar el frontend en `http://localhost:8080` y navegar por las vistas principales (`index.html`, `home.html`, `eventos.html`) para verificar enlaces y recursos.
2. Ejecutar el backend con `uvicorn` y consultar `GET /notifications/schedule` comprobando que refleja los valores de las variables de entorno configuradas.
3. Modificar temporalmente la ventana de la tarde y repetir la petición para confirmar que el horario se actualiza correctamente.

Consulta la guía completa de despliegue y hosting en `docs/DEPLOYMENT.md`.

## 🤝 Contribuir

1. Crea un fork del repositorio y una rama con un nombre descriptivo.
2. Realiza cambios siguiendo las buenas prácticas del lenguaje o herramienta utilizada.
3. Ejecuta `docker compose build` para confirmar que las imágenes se construyen correctamente.
4. Abre un Pull Request describiendo claramente los cambios e incluye los resultados de las pruebas relevantes.

Toda contribución es bienvenida. ¡Gracias por colaborar!
