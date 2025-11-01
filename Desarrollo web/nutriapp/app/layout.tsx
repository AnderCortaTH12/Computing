import type { Metadata } from "next";
import "./globals.css";
import { Inter } from "next/font/google";

const inter = Inter({ subsets: ["latin"], variable: "--font-inter" });

export const metadata: Metadata = {
  title: "NutriApp Insights",
  description: "Panel nutricional con navegación por pestañas y diseño accesible",
  applicationName: "NutriApp Insights"
};

export default function RootLayout({
  children
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="es" className={inter.variable}>
      <body>
        <div className="app-shell">{children}</div>
      </body>
    </html>
  );
}
