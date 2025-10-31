from functools import lru_cache
from pydantic import BaseSettings, Field


class Settings(BaseSettings):
    database_host: str = Field(default="db", env="POSTGRES_HOST")
    database_port: int = Field(default=5432, env="POSTGRES_PORT")
    database_user: str = Field(default="app_user", env="POSTGRES_USER")
    database_password: str = Field(default="app_password", env="POSTGRES_PASSWORD")
    database_name: str = Field(default="app_db", env="POSTGRES_DB")

    notification_morning_window: str = Field(
        default="08:00-09:00", env="NOTIFICATION_MORNING_WINDOW"
    )
    notification_evening_window: str = Field(
        default="18:00-19:00", env="NOTIFICATION_EVENING_WINDOW"
    )
    notification_timezone: str = Field(default="UTC", env="NOTIFICATION_TIMEZONE")

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"

    @property
    def database_url(self) -> str:
        return (
            f"postgresql+psycopg://{self.database_user}:{self.database_password}"
            f"@{self.database_host}:{self.database_port}/{self.database_name}"
        )


@lru_cache
def get_settings() -> Settings:
    return Settings()
