import { createContext, useCallback, useContext, useMemo, useReducer } from 'react';

const NotificationContext = createContext();

const initialState = [];

function reducer(state, action) {
  switch (action.type) {
    case 'ADD':
      return [...state, { ...action.payload }];
    case 'REMOVE':
      return state.filter((notification) => notification.id !== action.payload);
    case 'CLEAR':
      return [];
    default:
      return state;
  }
}

export function NotificationProvider({ children }) {
  const [notifications, dispatch] = useReducer(reducer, initialState);

  const showNotification = useCallback((notification) => {
    const id =
      notification.id ??
      (typeof crypto !== 'undefined' && crypto.randomUUID ? crypto.randomUUID() : `${Date.now()}-${Math.random()}`);
    dispatch({ type: 'ADD', payload: { id, ...notification } });
    if (!notification.persistent) {
      setTimeout(() => {
        dispatch({ type: 'REMOVE', payload: id });
      }, notification.duration ?? 5000);
    }
  }, []);

  const dismissNotification = useCallback((id) => {
    dispatch({ type: 'REMOVE', payload: id });
  }, []);

  const value = useMemo(
    () => ({ notifications, showNotification, dismissNotification }),
    [notifications, showNotification, dismissNotification]
  );

  return <NotificationContext.Provider value={value}>{children}</NotificationContext.Provider>;
}

export function useNotifications() {
  const context = useContext(NotificationContext);
  if (!context) {
    throw new Error('useNotifications must be used within a NotificationProvider');
  }
  return context;
}
