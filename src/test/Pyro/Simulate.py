import pyro
import pyro.distributions as dst
import numpy as np


def measurement(mean, sd):
    return pyro.sample('measurement_', dst.Normal(mean, sd))


def day_measurement(wake, sleep, y_one_mean, y_one_sd, y_two_mean, y_two_sd, y_three_mean, y_three_sd, times):
    measurements = np.empty(len(times))
    for i in range(0, len(times)):
        if times[i] < wake:
            mean = y_one_mean
            sd = y_one_sd
        elif times[i] < sleep:
            mean = y_two_mean
            sd = y_two_sd
        else:
            mean = y_three_mean
            sd = y_three_sd
        measurements[i] = measurement(mean, sd).item()
    return {'times': times, 'measurements': measurements}


def sleep_schedule(wake_mean, wake_sd, sleep_mean, sleep_sd):
    wake = pyro.sample('wake', dst.Normal(wake_mean, wake_sd))
    sleep = pyro.sample('sleep', dst.Normal(sleep_mean, sleep_sd))
    return {'wake': wake, 'sleep': sleep}


def means(one_mean, one_sd, two_mean, two_sd, three_mean, three_sd):
    one = pyro.sample('one', dst.Normal(one_mean, one_sd))
    two = pyro.sample('two', dst.Normal(two_mean, two_sd))
    three = pyro.sample('three', dst.Normal(three_mean, three_sd))
    return {'one': one, 'two': two, 'three': three}


def sds(one_sd_scale, two_sd_scale, three_sd_scale):
    one_sd = pyro.sample('one_sd', dst.HalfNormal(one_sd_scale))
    two_sd = pyro.sample('two_sd', dst.HalfNormal(two_sd_scale))
    three_sd = pyro.sample('three_sd', dst.HalfNormal(three_sd_scale))
    return {'one': one_sd, 'two': two_sd, 'three': three_sd}


def delta(pop_delta_mean, pop_delta_sd):
    delta_ = pyro.sample('delta_', dst.Normal(pop_delta_mean, pop_delta_sd))
    return {'delta': delta_}


def model(birds, days, points,
             window_one, window_two,
             pop_delta_mean, pop_delta_sd,
             pop_wake_mean, pop_wake_sd, pop_wake_sd_scale,
             pop_sleep_mean, pop_sleep_sd, pop_sleep_sd_scale,
             pop_mode_one_mean, pop_mode_one_sd, pop_y_one_sd_scale,
             pop_mode_two_mean, pop_mode_two_sd, pop_y_two_sd_scale,
             pop_mode_three_mean, pop_mode_three_sd, pop_y_three_sd_scale,
             pop_mode_one_sd_scale, pop_mode_two_sd_scale, pop_mode_three_sd_scale):

    out = {}

    for bird in range(0, birds):
        out[bird] = {}
        y_sds = sds(pop_y_one_sd_scale, pop_y_two_sd_scale, pop_y_three_sd_scale)
        y_one_sd = y_sds['one']
        y_two_sd = y_sds['two']
        y_three_sd = y_sds['three']

        delta_ = delta(pop_delta_mean, pop_delta_sd)
        interday_sleep_schedule = sleep_schedule(pop_wake_mean, pop_wake_sd, pop_sleep_mean, pop_sleep_sd)
        wake_mean = interday_sleep_schedule['wake']
        sleep_mean = interday_sleep_schedule['sleep']
        schedule_sds = sds(pop_wake_sd_scale, pop_sleep_sd_scale, 0)
        wake_sd = schedule_sds['one']
        sleep_sd = schedule_sds['two']

        mode_means = means(pop_mode_one_mean, pop_mode_one_sd, pop_mode_two_mean, pop_mode_two_sd, pop_mode_three_mean,
                           pop_mode_three_sd)
        mode_one_mean = mode_means['one']
        mode_two_mean = mode_means['two']
        mode_three_mean = mode_means['three']

        mode_sds = sds(pop_mode_one_sd_scale, pop_mode_two_sd_scale, pop_mode_three_sd_scale)
        mode_one_sd = mode_sds['one']
        mode_two_sd = mode_sds['two']
        mode_three_sd = mode_sds['three']

        for day in range(0, days):
            wake = pyro.sample('wake', dst.Normal(wake_mean, wake_sd))
            sleep = pyro.sample('sleep', dst.Normal(sleep_mean, sleep_sd))
            mode_one = pyro.sample('mode_one', dst.Normal(mode_one_mean, mode_one_sd))
            mode_two = pyro.sample('mode_two', dst.Normal(mode_two_mean, mode_two_sd))
            mode_three = pyro.sample('mode_three', dst.Normal(mode_three_mean, mode_three_sd))
            shift = dst.Normal(0, 1 / 4).sample()
            morning = np.linspace(window_one[0] + shift, window_one[0] + shift, points)
            evening = np.linspace(window_two[0] + shift, window_two[1] + shift, points)
            times = np.append(morning, evening)

            day_ = day_measurement(wake, sleep, mode_one, y_one_sd, mode_two, y_two_sd, mode_three, y_three_sd,
                                          times)

            out[bird][day] = day_

    return {'out': out}

def model_guide(



simulated_data = model(birds=10, days=10, points=60, window_one=[6, 9], window_two=[18, 21], pop_delta_mean=1 / 4,
         pop_delta_sd=1 / 10, pop_wake_mean=8, pop_wake_sd=1 / 4, pop_wake_sd_scale=1 / 8, pop_sleep_mean=20,
         pop_sleep_sd=1 / 4, pop_sleep_sd_scale=1 / 8, pop_mode_one_mean=50, pop_mode_one_sd=5, pop_y_one_sd_scale=1,
         pop_mode_two_mean=100, pop_mode_two_sd=10, pop_y_two_sd_scale=1, pop_mode_three_mean=50, pop_mode_three_sd=5,
         pop_y_three_sd_scale=1 / 8, pop_mode_one_sd_scale=1 / 8, pop_mode_two_sd_scale=1 / 8,
         pop_mode_three_sd_scale=1 / 8)



conditioned_model = pyro.condition(model, data = {'out' : simulated_data['out']})


