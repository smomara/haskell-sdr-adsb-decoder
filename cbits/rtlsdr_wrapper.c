#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <complex.h>
#include <rtl-sdr.h>
#include <errno.h>

#define DEFAULT_BUFFER_SIZE 256 * 1024  // 256k samples for better resolution
#define MAX_DEVICES 8

static rtlsdr_dev_t *dev = NULL;

int rtlsdr_open_device(int device_index) {
    int dev_count = rtlsdr_get_device_count();
    if (dev_count == 0) {
        fprintf(stderr, "No supported devices found.\n");
        return -1;
    }

    if (device_index < 0 || device_index >= dev_count) {
        fprintf(stderr, "Invalid device index. Available devices: %d\n", dev_count);
        return -1;
    }

    int ret = rtlsdr_open(&dev, device_index);
    if (ret < 0) {
        fprintf(stderr, "Failed to open rtlsdr device #%d: %s\n", device_index, strerror(errno));
        return -1;
    }

    printf("Opened device #%d: %s\n", device_index, rtlsdr_get_device_name(device_index));
    return 0;
}

int rtlsdr_setup_device(uint32_t sample_rate, uint32_t center_freq) {
    if (dev == NULL) {
        fprintf(stderr, "Device not opened. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int ret;

    // Set sample rate
    ret = rtlsdr_set_sample_rate(dev, sample_rate);
    if (ret < 0) {
        fprintf(stderr, "Failed to set sample rate to %u Hz: %s\n", sample_rate, strerror(errno));
        return -1;
    }

    // Set center frequency
    ret = rtlsdr_set_center_freq(dev, center_freq);
    if (ret < 0) {
        fprintf(stderr, "Failed to set center frequency to %u Hz: %s\n", center_freq, strerror(errno));
        return -1;
    }

    // Set automatic gain control
    ret = rtlsdr_set_tuner_gain_mode(dev, 0);
    if (ret < 0) {
        fprintf(stderr, "Failed to set automatic gain control: %s\n", strerror(errno));
        return -1;
    }

    // Reset buffer
    ret = rtlsdr_reset_buffer(dev);
    if (ret < 0) {
        fprintf(stderr, "Failed to reset buffer: %s\n", strerror(errno));
        return -1;
    }

    printf("Device set up successfully. Sample rate: %u Hz, Center frequency: %u Hz\n", sample_rate, center_freq);
    return 0;
}

int rtlsdr_set_gain(int gain) {
    if (dev == NULL) {
        fprintf(stderr, "Device not opened. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int ret;

    // Enable manual gain mode
    ret = rtlsdr_set_tuner_gain_mode(dev, 1);
    if (ret < 0) {
        fprintf(stderr, "Failed to enable manual gain mode: %s\n", strerror(errno));
        return -1;
    }

    // Set the gain
    ret = rtlsdr_set_tuner_gain(dev, gain);
    if (ret < 0) {
        fprintf(stderr, "Failed to set tuner gain: %s\n", strerror(errno));
        return -1;
    }

    printf("Gain set to %d tenths of dB\n", gain);
    return 0;
}

int rtlsdr_read_samples(unsigned char *buffer, int buffer_size) {
    if (dev == NULL) {
        fprintf(stderr, "Device not initialized. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int n_read;
    int ret = rtlsdr_read_sync(dev, buffer, buffer_size, &n_read);
    if (ret < 0) {
        fprintf(stderr, "rtlsdr_read_sync failed: %s\n", strerror(errno));
        return -1;
    }

    return n_read;
}

int rtlsdr_set_frequency(uint32_t frequency) {
    if (dev == NULL) {
        fprintf(stderr, "Device not initialized. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int ret = rtlsdr_set_center_freq(dev, frequency);
    if (ret < 0) {
        fprintf(stderr, "Failed to set center frequency to %u Hz: %s\n", frequency, strerror(errno));
        return -1;
    }

    printf("Center frequency set to %u Hz\n", frequency);
    return 0;
}

void rtlsdr_close_device() {
    if (dev != NULL) {
        rtlsdr_close(dev);
        dev = NULL;
        printf("RTL-SDR device closed\n");
    }
}

// Wrapper for rtlsdr_get_tuner_gains
int wrapper_rtlsdr_get_tuner_gains(int *gains) {
    if (dev == NULL) {
        fprintf(stderr, "Device not initialized. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int count = rtlsdr_get_tuner_gains(dev, NULL);
    if (count <= 0) {
        fprintf(stderr, "Failed to get tuner gains: %s\n", strerror(errno));
        return -1;
    }

    if (rtlsdr_get_tuner_gains(dev, gains) != count) {
        fprintf(stderr, "Failed to get tuner gains: %s\n", strerror(errno));
        return -1;
    }

    return count;
}

// Wrapper for rtlsdr_set_freq_correction
int wrapper_rtlsdr_set_freq_correction(int ppm) {
    if (dev == NULL) {
        fprintf(stderr, "Device not initialized. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int ret = rtlsdr_set_freq_correction(dev, ppm);
    if (ret < 0) {
        fprintf(stderr, "Failed to set frequency correction to %d ppm: %s\n", ppm, strerror(errno));
        return -1;
    }

    printf("Frequency correction set to %d ppm\n", ppm);
    return 0;
}

// Wrapper for rtlsdr_set_direct_sampling
int wrapper_rtlsdr_set_direct_sampling(int on) {
    if (dev == NULL) {
        fprintf(stderr, "Device not initialized. Call rtlsdr_open_device first.\n");
        return -1;
    }

    int ret = rtlsdr_set_direct_sampling(dev, on);
    if (ret < 0) {
        fprintf(stderr, "Failed to set direct sampling mode: %s\n", strerror(errno));
        return -1;
    }

    printf("Direct sampling mode %s\n", on ? "enabled" : "disabled");
    return 0;
}
