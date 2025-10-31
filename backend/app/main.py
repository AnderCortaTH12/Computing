from fastapi import FastAPI

from .config import get_settings

app = FastAPI(title="Computing Backend")


@app.get("/health", summary="Comprobar el estado del servicio")
def read_health():
    settings = get_settings()
    return {
        "status": "ok",
        "database_url": settings.database_url,
        "notification_timezone": settings.notification_timezone,
    }


@app.get(
    "/notifications/schedule",
    summary="Obtener ventanas horarias configuradas para las notificaciones",
)
def read_notification_schedule():
    settings = get_settings()
    return {
        "morning": settings.notification_morning_window,
        "evening": settings.notification_evening_window,
        "timezone": settings.notification_timezone,
    }
