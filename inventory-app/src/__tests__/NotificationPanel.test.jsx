import { render, screen, fireEvent } from '@testing-library/react';
import NotificationPanel from '../components/NotificationPanel.jsx';

describe('NotificationPanel', () => {
  const notifications = [
    {
      id: '1',
      title: 'Reponer leche',
      message: 'El stock de leche entera está por debajo del mínimo.',
      date: new Date().toISOString(),
      status: 'pending'
    }
  ];

  it('renderiza la lista de notificaciones', () => {
    render(<NotificationPanel notifications={notifications} onAcknowledge={jest.fn()} />);

    expect(screen.getByText(/reponer leche/i)).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /marcar como atendida/i })).toBeInTheDocument();
    expect(screen.getByText(/pendiente/i)).toBeInTheDocument();
  });

  it('ejecuta la acción de acknowledge al pulsar el botón', () => {
    const onAcknowledge = jest.fn();
    render(<NotificationPanel notifications={notifications} onAcknowledge={onAcknowledge} />);

    fireEvent.click(screen.getByRole('button', { name: /marcar como atendida/i }));

    expect(onAcknowledge).toHaveBeenCalledWith('1');
  });
});
