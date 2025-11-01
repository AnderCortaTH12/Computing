from datetime import time

import pytest

from backend.app import config
from backend.app.notifications import NotificationWindow, build_schedule, parse_window


def test_parse_window_success():
    window = parse_window("08:30-10:15")
    assert window == NotificationWindow(start=time(8, 30), end=time(10, 15))


def test_parse_window_invalid_format():
    with pytest.raises(ValueError):
        parse_window("0830-1015")


def test_parse_window_start_after_end():
    with pytest.raises(ValueError):
        parse_window("18:00-08:00")


def test_build_schedule_uses_timezone():
    schedule = build_schedule(
        timezone="Europe/Madrid",
        windows=("08:00-09:00", "18:00-19:00"),
    )

    assert schedule.morning.start == time(8, 0)
    assert schedule.evening.end == time(19, 0)
    assert schedule.timezone == "Europe/Madrid"


def test_load_settings_from_env(monkeypatch):
    env = {
        "NOTIFICATION_MORNING_WINDOW": "06:00-07:00",
        "NOTIFICATION_EVENING_WINDOW": "21:00-22:00",
        "NOTIFICATION_TIMEZONE": "UTC",
    }

    settings = config.load_settings(env)

    assert settings.morning_window == "06:00-07:00"
    assert settings.evening_window == "21:00-22:00"
    assert settings.timezone == "UTC"


def test_get_schedule_uses_cached_settings(monkeypatch):
    monkeypatch.setenv("NOTIFICATION_MORNING_WINDOW", "07:00-08:00")
    monkeypatch.setenv("NOTIFICATION_EVENING_WINDOW", "19:00-20:00")
    monkeypatch.setenv("NOTIFICATION_TIMEZONE", "Europe/Paris")
    config.get_settings.cache_clear()

    schedule = config.get_schedule()

    assert schedule.morning.start == time(7, 0)
    assert schedule.evening.start == time(19, 0)
    assert schedule.timezone == "Europe/Paris"
