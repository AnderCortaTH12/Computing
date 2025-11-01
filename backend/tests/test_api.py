from fastapi.testclient import TestClient

from backend.app import config
from backend.app.main import app


def test_health_endpoint_returns_ok():
    client = TestClient(app)
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json() == {"status": "ok"}


def test_notifications_schedule_endpoint(monkeypatch):
    monkeypatch.setenv("NOTIFICATION_MORNING_WINDOW", "06:30-08:00")
    monkeypatch.setenv("NOTIFICATION_EVENING_WINDOW", "18:30-20:00")
    monkeypatch.setenv("NOTIFICATION_TIMEZONE", "Europe/Madrid")
    config.get_settings.cache_clear()

    client = TestClient(app)
    response = client.get("/notifications/schedule")

    assert response.status_code == 200
    assert response.json() == {
        "timezone": "Europe/Madrid",
        "morning": {"start": "06:30:00", "end": "08:00:00"},
        "evening": {"start": "18:30:00", "end": "20:00:00"},
    }
