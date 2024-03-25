using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AudioManager : BaseMonoClass<AudioManager>
{
    public AudioSource audio;
    private bool isOpen;
    protected override void Awake()
    {
        base.Awake();
        isOpen = false;
        audio = GetComponent<AudioSource>();
    }
    private void Update()
    {
        if (Input.GetKeyDown(KeyCode.O))
        {
            if (isOpen)
            {
                CloseAudio();
            }
            else
            {
                OpenAudio();
            }
        }
    }
    public void OpenAudio()
    {
        audio.Play();
        isOpen = true;
    }
    public void CloseAudio()
    {
        audio.Stop();
        isOpen = false;
    }
}
