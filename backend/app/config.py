"""Configuración de la aplicación FastAPI."""
from __future__ import annotations

import os
from functools import lru_cache
from typing import Mapping, MutableMapping

from pydantic import BaseModel, Field

from .notifications import NotificationSchedule, build_schedule


class Settings(BaseModel):
    """Valores configurables mediante variables de entorno."""

    morning_window: str = Field(default="08:00-12:00")
    evening_window: str = Field(default="17:00-20:00")
    timezone: str = Field(default="Europe/Madrid")

    def schedule(self) -> NotificationSchedule:
        return build_schedule(
            timezone=self.timezone,
            windows=(self.morning_window, self.evening_window),
        )


ENV_MAPPING: Mapping[str, str] = {
    "morning_window": "NOTIFICATION_MORNING_WINDOW",
    "evening_window": "NOTIFICATION_EVENING_WINDOW",
    "timezone": "NOTIFICATION_TIMEZONE",
}


def load_settings(environ: MutableMapping[str, str] | None = None) -> Settings:
    """Crea ``Settings`` a partir de un diccionario estilo ``os.environ``."""

    environ = environ or os.environ
    data: dict[str, str] = {}

    for field_name, env_name in ENV_MAPPING.items():
        value = environ.get(env_name)
        if value:
            data[field_name] = value

    return Settings(**data)


@lru_cache
def get_settings() -> Settings:
    """Devuelve la configuración cargada desde las variables de entorno."""

    return load_settings()


def get_schedule() -> NotificationSchedule:
    """Acceso auxiliar para reutilizar la lógica en los endpoints."""

    return get_settings().schedule()
