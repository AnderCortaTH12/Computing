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
"""FastAPI application exposing profile endpoints for the nutrition app."""
from __future__ import annotations

import os

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from .api import profile
from .database import init_db

app = FastAPI(title="Nutrition Profile API", version="0.1.0")

allowed_origins = [origin.strip() for origin in os.getenv("CORS_ALLOWED_ORIGINS", "*").split(",")]

app.add_middleware(
    CORSMiddleware,
    allow_origins=allowed_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

init_db()
app.include_router(profile.router)


@app.get("/health", tags=["health"]) 
def health_check() -> dict[str, str]:
    """Simple endpoint to confirm the service is running."""
    return {"status": "ok"}
