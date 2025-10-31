import { NavLink, Outlet } from 'react-router-dom';
import NotificationToasts from './NotificationToasts.jsx';
import './Layout.css';

export default function Layout() {
  return (
    <div className="app-shell">
      <aside className="sidebar">
        <h1 className="logo">Inventario</h1>
        <nav>
          <NavLink to="/" end className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Dashboard
          </NavLink>
          <NavLink to="/productos/nuevo" className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Nuevo producto
          </NavLink>
          <NavLink to="/recetas" className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Sugerencias
          </NavLink>
          <NavLink to="/notificaciones" className={({ isActive }) => (isActive ? 'active' : undefined)}>
            Notificaciones
          </NavLink>
        </nav>
      </aside>
      <main className="content">
        <Outlet />
      </main>
      <NotificationToasts />
    </div>
  );
}
