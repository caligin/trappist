FROM xdrum/openwrt
MAINTAINER Tsukihara Caligin <caligin35+docker@gmail.com>

RUN opkg update && opkg install openssh-server shadow-useradd
RUN mkdir /var/empty && \
    useradd sshd && \
    ssh-keygen -N '' -t rsa -f /etc/ssh/ssh_host_rsa_key
    
RUN useradd ackbar && \
    echo -e "trap\ntrap" | busybox passwd ackbar

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
