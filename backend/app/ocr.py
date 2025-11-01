from __future__ import annotations

import io
from typing import List, Tuple

from fastapi import HTTPException, UploadFile
from PIL import Image

try:
    import pytesseract
    from pytesseract import TesseractNotFoundError
except ImportError:  # pragma: no cover - handled gracefully at runtime
    pytesseract = None  # type: ignore
    TesseractNotFoundError = RuntimeError  # type: ignore


def extract_text_from_bytes(data: bytes) -> Tuple[str, List[str]]:
    warnings: List[str] = []
    try:
        image = Image.open(io.BytesIO(data))
    except Exception as exc:  # pragma: no cover - protective branch
        raise HTTPException(status_code=400, detail=f"No se pudo leer la imagen: {exc}") from exc

    if pytesseract is None:
        warnings.append(
            "pytesseract no está instalado. Instala el paquete y el binario 'tesseract-ocr' para habilitar el escaneo."
        )
        return "", warnings

    try:
        text = pytesseract.image_to_string(image)
    except TesseractNotFoundError as exc:  # pragma: no cover - depende del entorno
        warnings.append(
            "No se encontró el ejecutable de Tesseract en el sistema. Instala 'tesseract-ocr' y configura la variable"
            " TESSERACT_CMD si es necesario."
        )
        text = ""
    return text, warnings


async def extract_text_from_upload(file: UploadFile) -> Tuple[str, List[str]]:
    data = await file.read()
    if not data:
        raise HTTPException(status_code=400, detail="El archivo recibido está vacío")
    return extract_text_from_bytes(data)
