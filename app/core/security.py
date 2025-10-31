from typing import Dict

from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBasic, HTTPBasicCredentials


security = HTTPBasic()

# In a real application this would be replaced with a persistent store or
# proper password hashing. Here we keep it simple for demonstration
# purposes.
_USERS: Dict[str, str] = {"admin": "secret"}


def verify_credentials(credentials: HTTPBasicCredentials) -> str:
    """Validate the provided HTTP Basic credentials.

    Returns the username if the credentials are valid, otherwise raises a
    401 error.
    """

    correct_password = _USERS.get(credentials.username)
    if not correct_password or credentials.password != correct_password:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid authentication credentials",
            headers={"WWW-Authenticate": "Basic"},
        )
    return credentials.username


def get_current_username(credentials: HTTPBasicCredentials = Depends(security)) -> str:
    """FastAPI dependency that ensures the request is authenticated."""

    return verify_credentials(credentials)
