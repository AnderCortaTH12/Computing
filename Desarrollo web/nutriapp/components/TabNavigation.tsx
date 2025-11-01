"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import { FiBarChart2, FiFridge, FiHome, FiSettings, FiStar } from "react-icons/fi";

const tabs = [
  {
    href: "/dashboard",
    label: "Diario",
    description: "Resumen diario",
    icon: FiHome
  },
  {
    href: "/fridge",
    label: "Nevera",
    description: "Inventario inteligente",
    icon: FiFridge
  },
  {
    href: "/stats",
    label: "Estadísticas",
    description: "Tendencias y métricas",
    icon: FiBarChart2
  },
  {
    href: "/tips",
    label: "Tips",
    description: "Recomendaciones personalizadas",
    icon: FiStar
  },
  {
    href: "/settings",
    label: "Configuración",
    description: "Preferencias de la app",
    icon: FiSettings
  }
] as const;

export default function TabNavigation() {
  const pathname = usePathname();

  return (
    <nav className="tab-navigation" aria-label="Secciones principales">
      {tabs.map((tab) => {
        const Icon = tab.icon;
        const isActive = pathname === tab.href || pathname.startsWith(`${tab.href}/`);

        return (
          <Link
            key={tab.href}
            href={tab.href}
            className="tab-navigation__link"
            aria-current={isActive ? "page" : undefined}
            aria-label={`${tab.label}. ${tab.description}`}
          >
            <Icon aria-hidden="true" focusable="false" />
            <span>{tab.label}</span>
          </Link>
        );
      })}
    </nav>
  );
}
