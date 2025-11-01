"use client";

import { useMemo, useState } from "react";
import { FiActivity, FiCheckCircle, FiClock, FiPlusCircle } from "react-icons/fi";

type FeedbackType = "meal" | "training" | null;

type ActionState = {
  status: FeedbackType;
  timestamp: number | null;
};

const FEEDBACK_DURATION = 3200;

export default function QuickLogActions() {
  const [{ status, timestamp }, setActionState] = useState<ActionState>({
    status: null,
    timestamp: null
  });

  const handleAction = (type: Exclude<FeedbackType, null>) => {
    const nextTimestamp = Date.now();
    setActionState({ status: type, timestamp: nextTimestamp });

    window.setTimeout(() => {
      setActionState((prev) =>
        prev.timestamp === nextTimestamp ? { status: null, timestamp: null } : prev
      );
    }, FEEDBACK_DURATION);
  };

  const feedbackMessage = useMemo(() => {
    if (status === "meal") {
      return "¡Comida registrada! Tu dashboard ya refleja el cambio.";
    }

    if (status === "training") {
      return "Entrenamiento añadido. Ajustamos tus objetivos en tiempo real.";
    }

    return null;
  }, [status]);

  return (
    <div className="surface-card surface-card--highlight" aria-live="polite">
      <div className="badge">Acciones rápidas</div>
      <h2>Registrar actividad</h2>
      <p className="surface-card__subtitle">
        Añade comidas o entrenamientos y obtén feedback instantáneo para mantener tus hábitos al día.
      </p>
      <div className="actions-row" role="group" aria-label="Registrar comidas o entrenamientos">
        <button
          type="button"
          className="button button--primary"
          onClick={() => handleAction("meal")}
        >
          <FiPlusCircle aria-hidden="true" />
          Registrar comida
        </button>
        <button
          type="button"
          className="button button--ghost"
          onClick={() => handleAction("training")}
        >
          <FiActivity aria-hidden="true" />
          Añadir entrenamiento
        </button>
      </div>
      {status && feedbackMessage ? (
        <div className="feedback-banner" role="status">
          <FiCheckCircle aria-hidden="true" />
          <span>{feedbackMessage}</span>
          <span className="surface-card__subtitle" style={{ display: "inline-flex", gap: "0.35rem" }}>
            <FiClock aria-hidden="true" /> hace un momento
          </span>
        </div>
      ) : null}
    </div>
  );
}
