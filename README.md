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

El directorio `Desarrollo web/` contiene el sitio estático. Puedes servirlo con cualquier servidor de archivos estáticos. Un ejemplo rápido utilizando `python -m http.server`:

```bash
cd "Desarrollo web"
python -m http.server 8080
```

### Variables de entorno relevantes

| Variable | Descripción |
|----------|-------------|
| `NOTIFICATION_MORNING_WINDOW`, `NOTIFICATION_EVENING_WINDOW` | Ventanas horarias (formato `HH:MM-HH:MM`) utilizadas por el backend para programar notificaciones. |
| `NOTIFICATION_TIMEZONE` | Zona horaria que se empleará al interpretar las ventanas configuradas. |

El backend expone el endpoint `GET /notifications/schedule` para consultar los valores actuales de estas variables.

## 🧪 Pruebas

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
