function Main {
    try {
        # Windows 버전 및 빌드 번호 확인
        $osInfo = Get-WmiObject -Class Win32_OperatingSystem
        $windowsVersion = [System.Version]$osInfo.Version
        $windowsBuild = $osInfo.BuildNumber

        if ($windowsVersion.Major -ge 10 -and $windowsVersion.Minor -ge 0 -and $windowsBuild -ge 19041) {
            Write-Host "Compatible Windows version detected (Windows 10 version 2004 or later, or Windows 11)."
            Write-Host "Activating WSL and Virtual Machine Platform features..."
            dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
            dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

            Write-Host "Installing WSL with Ubuntu..."
            wsl --install
            
            Write-Host "WSL and default Ubuntu distribution have been successfully installed."
            Write-Host "A system restart is required to complete the installation."
            
            $restart = Read-Host "Do you want to restart now? (Y/N)"
            if ($restart -eq 'Y' -or $restart -eq 'y') {
                Restart-Computer
            }
        } else {
            # 호환되지 않는 버전인 경우
            Write-Host "Incompatible Windows version detected. Proceeding with WSL installation and kernel update."

            Write-Host "Activating WSL and Virtual Machine Platform features..."
            dism.exe /online /enable-feature /featurename:Microsoft-Windows-Subsystem-Linux /all /norestart
            dism.exe /online /enable-feature /featurename:VirtualMachinePlatform /all /norestart

            Write-Host "WSL is already installed. Proceeding with WSL2 kernel update..."

            # WSL2 커널 업데이트
            Write-Host "Downloading and installing WSL2 Linux kernel update package..."
            $url = "https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi"
            if ($env:PROCESSOR_ARCHITECTURE -like "*ARM*") {
                $url = "https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_arm64.msi"
            }

            $outPath = "$env:TEMP\wsl_update.msi"
            try {
                Invoke-WebRequest -Uri $url -OutFile $outPath -ErrorAction Stop
                Write-Host "WSL2 Linux kernel update package downloaded successfully."
            } catch {
                Write-Warning "Failed to download WSL2 Linux kernel update package: $_"
                Write-Host "Please download and install the WSL2 Linux kernel update package manually from: $url"
                return
            }

            try {
                $installProcess = Start-Process msiexec.exe -Wait -ArgumentList "/I `$outPath /quiet" -PassThru -Verb RunAs
                if ($installProcess.ExitCode -eq 0) {
                    Write-Host "WSL2 Linux kernel update package installed successfully."
                } else {
                    Write-Warning "WSL2 Linux kernel update package installation might have failed. Exit code: $($installProcess.ExitCode)"
                }
            } catch {
                Write-Warning "An error occurred during WSL2 Linux kernel update package installation: $_"
            }

            # Ubuntu 설치 여부 확인 및 설치
            if (-not (wsl --list --online | Select-String -Pattern "Ubuntu")) {
                try {
                    Write-Host "Attempting to install Ubuntu 22.04.3 LTS..."
                    $installProcess = Start-Process -FilePath "winget" -ArgumentList "install --id 9PN20MSR04DW --source msstore --accept-package-agreements --accept-source-agreements" -NoNewWindow -PassThru -Wait

                    if ($installProcess.ExitCode -eq 0) {
                        Write-Host "Ubuntu 22.04.3 LTS has been successfully installed."
                    } else {
                        Write-Warning "Installation might have failed. Exit code: $($installProcess.ExitCode)"
                        Write-Host "If the installation failed, please try to install Ubuntu 22.04.3 LTS manually from the Microsoft Store."
                    }
                } catch {
                    Write-Warning "An error occurred during installation: $_"
                    Write-Host "Please try to install Ubuntu 22.04.3 LTS manually from the Microsoft Store."
                }
            } else {
                Write-Host "Ubuntu is already installed."
            }

            Write-Host "To run Ubuntu, search for 'Ubuntu' in the Start menu."

            # 재시작 요청
            $restart = Read-Host "A system restart is recommended to complete the update. Do you want to restart now? (Y/N)"
            if ($restart -eq 'Y' -or $restart -eq 'y') {
                Restart-Computer
            }
        }
    } catch {
        Write-Host "An error occurred: $_"
        Write-Host "Please check the error and try again."
    }
}

# 스크립트 실행
Main
