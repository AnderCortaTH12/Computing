#!/usr/bin/env bash
set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [ ! -f "$PROJECT_DIR/.env" ]; then
  echo "[deploy] No se encontró un archivo .env. Copia .env.example y ajusta las credenciales antes de desplegar." >&2
  exit 1
fi

cd "$PROJECT_DIR"

echo "[deploy] Construyendo imágenes..."
docker compose build

echo "[deploy] Aplicando migraciones pendientes (placeholder)..."
# Aquí se podrían ejecutar migraciones de la base de datos, por ejemplo con Alembic o Prisma.


echo "[deploy] Levantando servicios en modo desprendido..."
docker compose up -d

echo "[deploy] Despliegue completado."
