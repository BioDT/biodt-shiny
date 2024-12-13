document.addEventListener('DOMContentLoaded', function() {
        document.getElementById('toggleSidebar').addEventListener('click', function() {
          var sidebar = document.getElementById('sidebar');
          if (sidebar.style.display === 'none') {
            sidebar.style.display = 'block';
          } else {
            sidebar.style.display = 'none';
          }
        });
      });
