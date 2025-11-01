# Guía de despliegue

Esta guía describe cómo publicar la aplicación en plataformas gratuitas tanto para el backend (FastAPI) como para el frontend estático.

## Backend en Render

1. Crea una cuenta gratuita en [Render](https://render.com/).
2. Importa el repositorio desde GitHub y elige la rama que quieras desplegar.
3. Configura el servicio con los siguientes valores:
   - **Type**: Web Service.
   - **Runtime**: Python.
   - **Build Command**: `pip install -r requirements.txt`.
   - **Start Command**: `uvicorn backend.app.main:app --host 0.0.0.0 --port 10000`.
   - **Environment**: agrega las variables `NOTIFICATION_MORNING_WINDOW`, `NOTIFICATION_EVENING_WINDOW` y `NOTIFICATION_TIMEZONE` según tus necesidades. Si no las configuras se usarán los valores por defecto documentados en `backend/app/config.py`.
4. Render detectará automáticamente el archivo `render.yaml` incluido en el repositorio, por lo que los valores anteriores quedarán registrados y los despliegues posteriores serán automáticos tras cada push a `main`.

## Frontend en Netlify

1. Accede a [Netlify](https://app.netlify.com/) y selecciona **Add new site → Import an existing project**.
2. Autoriza a Netlify a leer este repositorio.
3. Cuando se te solicite la configuración de build indica:
   - **Build command**: *(vacío, no es necesario compilar)*.
   - **Publish directory**: `Desarrollo web`.
4. Netlify utilizará el archivo `netlify.toml` del repositorio para recordar estas opciones y crear un entorno de previsualización por cada pull request.

## Actualizar variables de entorno

En ambos servicios podrás definir las variables de entorno desde sus paneles de configuración. Tras modificar alguna ventana de notificación, solicita un redeploy manual para que la API lea los nuevos valores.

## Checklist previo al despliegue

- Ejecutar `ruff check .` y `pytest` en local.
- Revisar que las rutas críticas de la web sigan cargando en el entorno de staging de Netlify.
- Confirmar desde Render que el endpoint `GET /notifications/schedule` devuelve las ventanas esperadas.
