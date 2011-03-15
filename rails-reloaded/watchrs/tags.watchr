watch(/\.el/) { |md| system "ctags -R -e .; sleep 300" }
