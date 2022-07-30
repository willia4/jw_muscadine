function toggleMenu() {
  const sidebar = document.getElementById("main-sidebar");
  if (!sidebar) { return; }
  const sidebarClasses = Array.from(sidebar.classList)

  if (sidebarClasses.indexOf('shown') >= 0) {
    sidebar.classList.remove('shown');
  }
  else {
    sidebar.classList.add('shown');
  }
}

(() => {
  if (document.addEventListener) {
      document.addEventListener("DOMContentLoaded", () => {
          const menuButton = Array.from(document.getElementsByClassName("menu-button"));
          menuButton.forEach(b => b.addEventListener("click", (event) => { toggleMenu(); }));
          ;
    });
  }
})();
