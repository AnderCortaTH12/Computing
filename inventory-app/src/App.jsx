import { Routes, Route } from 'react-router-dom';
import Layout from './components/Layout.jsx';
import DashboardPage from './pages/DashboardPage.jsx';
import NewProductPage from './pages/NewProductPage.jsx';
import RecipesPage from './pages/RecipesPage.jsx';
import NotificationsPage from './pages/NotificationsPage.jsx';

export default function App() {
  return (
    <Routes>
      <Route element={<Layout />}>
        <Route index element={<DashboardPage />} />
        <Route path="productos/nuevo" element={<NewProductPage />} />
        <Route path="recetas" element={<RecipesPage />} />
        <Route path="notificaciones" element={<NotificationsPage />} />
      </Route>
    </Routes>
  );
}
