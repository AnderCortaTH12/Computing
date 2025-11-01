"""Entrypoint de la API FastAPI."""
from fastapi import FastAPI, HTTPException

from .config import get_schedule
from .notifications import NotificationSchedule

app = FastAPI(title="Notificaciones Sureuskadi", version="1.0.0")


@app.get("/health")
def health_check() -> dict[str, str]:
    return {"status": "ok"}


@app.get("/notifications/schedule", response_model=NotificationSchedule)
def read_schedule() -> NotificationSchedule:
    try:
        return get_schedule()
    except ValueError as exc:
        raise HTTPException(status_code=500, detail=str(exc)) from exc
