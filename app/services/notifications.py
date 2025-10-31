from __future__ import annotations

from datetime import datetime
from typing import List

from apscheduler.schedulers.asyncio import AsyncIOScheduler

_notifications: List[str] = []
scheduler = AsyncIOScheduler()


def _send_notification(message: str) -> None:
    timestamp = datetime.utcnow().isoformat()
    entry = f"[{timestamp}] {message}"
    _notifications.append(entry)
    # In a real system this might send an email, push notification, etc.
    print(entry)


def schedule_notifications() -> None:
    if scheduler.get_jobs():
        return

    scheduler.add_job(
        _send_notification,
        "cron",
        hour=13,
        minute=30,
        args=["Recordatorio de inventario (13:30)"],
        id="notification_1330",
        replace_existing=True,
    )
    scheduler.add_job(
        _send_notification,
        "cron",
        hour=20,
        minute=15,
        args=["Recordatorio de inventario (20:15)"],
        id="notification_2015",
        replace_existing=True,
    )


async def start_scheduler() -> None:
    if not scheduler.running:
        schedule_notifications()
        scheduler.start()


async def shutdown_scheduler() -> None:
    if scheduler.running:
        scheduler.shutdown()


def get_notifications() -> List[str]:
    return list(_notifications)


def clear_notifications() -> None:
    _notifications.clear()
