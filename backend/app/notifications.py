"""Notification scheduling logic for the FastAPI backend."""
from __future__ import annotations

from datetime import time
from typing import Iterable

from pydantic import BaseModel


class NotificationWindow(BaseModel):
    """Time window where notifications can be sent."""

    start: time
    end: time

    model_config = {"frozen": True}


class NotificationSchedule(BaseModel):
    """Full schedule returned by the API."""

    timezone: str
    morning: NotificationWindow
    evening: NotificationWindow

    model_config = {"frozen": True}


def _parse_time_component(component: str, *, field: str) -> time:
    try:
        hour_str, minute_str = component.split(":", maxsplit=1)
    except ValueError as exc:  # pragma: no cover - defensive branch
        raise ValueError(f"{field} debe tener el formato HH:MM") from exc

    if not hour_str.isdigit() or not minute_str.isdigit():
        raise ValueError(f"{field} debe contener valores numéricos")

    hour = int(hour_str)
    minute = int(minute_str)

    if hour not in range(0, 24) or minute not in range(0, 60):
        raise ValueError(f"{field} debe estar en un rango válido (00:00-23:59)")

    return time(hour=hour, minute=minute)


def parse_window(window: str) -> NotificationWindow:
    """Parsea una ventana en formato ``HH:MM-HH:MM``."""

    if "-" not in window:
        raise ValueError("Las ventanas deben usar el formato HH:MM-HH:MM")

    start_raw, end_raw = window.split("-", maxsplit=1)
    start = _parse_time_component(start_raw.strip(), field="Inicio")
    end = _parse_time_component(end_raw.strip(), field="Fin")

    if start >= end:
        raise ValueError("La hora de inicio debe ser menor que la hora de fin")

    return NotificationWindow(start=start, end=end)


def build_schedule(*, timezone: str, windows: Iterable[str]) -> NotificationSchedule:
    morning_window, evening_window = list(windows)
    return NotificationSchedule(
        timezone=timezone,
        morning=parse_window(morning_window),
        evening=parse_window(evening_window),
    )
