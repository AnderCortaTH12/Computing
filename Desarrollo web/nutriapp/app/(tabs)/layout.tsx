import TabNavigation from "@/components/TabNavigation";

export default function TabsLayout({
  children
}: {
  children: React.ReactNode;
}) {
  return (
    <>
      <TabNavigation />
      <main>{children}</main>
    </>
  );
}
